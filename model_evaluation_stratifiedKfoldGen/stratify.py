#create a new conda env for this via terminal
#module load conda/latest
#conda create -n stratify-env
#conda activate stratify-env #Python 3.13.2
#conda install pandas scikit-learn
#pip install pycocotools
#conda env export --no-builds > stratify-env.yml
#you might have to reset the python interpreter location in vscode
#next time do this analysis in a jupyter notebook, makes more sense with viewing dfs

# libraries
import os
import sys
import json
import pandas as pd
from sklearn.model_selection import StratifiedKFold

print(os.getcwd())

# import all coco json annotation files from ../annotations/train/_annotations.coco.json, and ../annotations/valid/_annotations.coco.json
annotation_paths = [
    "../annotations/train/_annotations.coco.json",
    "../annotations/valid/_annotations.coco.json",
    "../annotations/test/_annotations.coco.json"
]
all_filenames = []
print(annotation_paths)
# extract all of the filenames from each file (using "file_name" as the unique identifier)
for path in annotation_paths:
    with open(path, 'r') as f:
        data = json.load(f)
        image_filenames = [img["file_name"] for img in data["images"]]
        all_filenames.extend(image_filenames)

df = pd.DataFrame({"file_name": all_filenames})
df["pistil_type"] = ""  # placeholder column to be manually filled in later #pistil_type_species_size_genotype_stage

# view duplicates just in case (there should not be any)
print("duplicates?")
print(df[df.duplicated("file_name", keep=False)])

# sort alphabetically by file name
df = df.sort_values(by="file_name").reset_index(drop=True)

print("does df look okay?")
print(df)

# write out a csv with file names, will manually add subcategory data to the second column
df.to_csv("pistil_types.csv", index=False)

# read csv back in with new data
df_with_types = pd.read_csv("pistil_types_mod.csv")
print(df_with_types)
print(df_with_types["pistil_type_species_size_genotype_stage"].value_counts())

#[591 rows x 2 columns]
#pistil_type_species_size_genotype_stage
#Zeamays_medium_gt1ra3_anthesis             151
#Setariaviridis_large_A10svra3_33DAS         88
#Zeamays_nonesmall_gt1P_anthesis             81
#Setariaviridis_nonesmall_A10svra3_21DAS     72
#Setariaviridis_medium_A10svra3_31DAS        67
#Setariaviridis_xlarge_A10svra3_42DAS        58
#Zeamays_large_tb1gt1ra3hybrid_anthesis      40
#Zeamays_none_B73_anthesis                   34

file_to_type = dict(zip(df_with_types["file_name"], df_with_types["pistil_type_species_size_genotype_stage"]))

# load full COCO dataset
images = []
annotations = []

# load and merge all images/annotations from each COCO file
for idx, path in enumerate(annotation_paths):
    with open(path, "r") as f:
        coco_part = json.load(f)
    # Offset to ensure unique IDs (e.g. 1000, 2000, 3000...)
    image_offset = (idx + 1) * 1000
    # Update image IDs
    for img in coco_part["images"]:
        img["id"] += image_offset
    for ann in coco_part["annotations"]:
        ann["image_id"] += image_offset
    images.extend(coco_part["images"])
    annotations.extend(coco_part["annotations"])

print(f"Total images: {len(images)}")
print(f"Total annotations loaded: {len(annotations)}")
file_names = [img["file_name"] for img in images]
types = [file_to_type[fn] for fn in file_names]


# Build folds
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=33)
  
for fold, (train_idx, val_idx) in enumerate(skf.split(file_names, types)):
    train_file_names = set([file_names[i] for i in train_idx])
    val_file_names = set([file_names[i] for i in val_idx])
    
    train_images = [img for img in images if img["file_name"] in train_file_names]
    val_images = [img for img in images if img["file_name"] in val_file_names]
    
    train_ids = {img["id"] for img in train_images}
    val_ids = {img["id"] for img in val_images}
    
    train_annots = [a for a in annotations if a["image_id"] in train_ids]
    val_annots = [a for a in annotations if a["image_id"] in val_ids]
   
    category = [{"id": 1, "name": "carpel", "supercategory": "pistil"}]
    os.makedirs(f"annotations/fold_{fold}", exist_ok=True)
    
    with open(f"annotations/fold_{fold}/train.json", "w") as f:
        json.dump({"images": train_images, "annotations": train_annots, "categories": category}, f)

    with open(f"annotations/fold_{fold}/val.json", "w") as f:
        json.dump({"images": val_images, "annotations": val_annots, "categories": category}, f)

    # create and save a CSV summary of file names and labels
    fold_summary_dir = f"annotations/fold_{fold}"
    
    # Get file names and labels for train
    train_summary = pd.DataFrame({
        "file_name": [img["file_name"] for img in train_images],
        "label": [file_to_type[img["file_name"]] for img in train_images]
    })
    train_summary.to_csv(os.path.join(fold_summary_dir, "train_summary.csv"), index=False)

    # Get file names and labels for val
    val_summary = pd.DataFrame({
        "file_name": [img["file_name"] for img in val_images],
        "label": [file_to_type[img["file_name"]] for img in val_images]
    })
    val_summary.to_csv(os.path.join(fold_summary_dir, "val_summary.csv"), index=False)
import os
import json

base_dir = "/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/stratifiedKfoldGen/annotations"

# Iterate over all fold directories
for fold in range(5):
    for split in ["train", "val"]:
        json_path = os.path.join(base_dir, f"fold_{fold}", f"{split}.json")
        
        with open(json_path, "r") as f:
            data = json.load(f)

        # Reassign unique annotation IDs
        for new_id, ann in enumerate(data["annotations"]):
            ann["id"] = new_id

        with open(json_path, "w") as f:
            json.dump(data, f, indent=2)

        print(f"✅ Updated annotation IDs in: {json_path}")

# Build a DataFrame to track which fold each image is in
image_fold_matrix = pd.DataFrame(index=[img["file_name"] for img in images])

# Re-run the stratified split to fill in the matrix
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=33)

for fold, (train_idx, val_idx) in enumerate(skf.split(file_names, types)):
    fold_col = f"fold_{fold}"
    fold_membership = pd.Series(False, index=image_fold_matrix.index)

    val_file_names = [file_names[i] for i in val_idx]
    fold_membership.loc[val_file_names] = True

    image_fold_matrix[fold_col] = fold_membership

image_fold_matrix.to_csv("annotations/image_fold_membership.csv")

#check fold percentages
from collections import defaultdict

# Set up a dictionary to hold label counts per fold
fold_label_counts = defaultdict(lambda: defaultdict(int))
total_label_counts = pd.Series(types).value_counts()

# Re-run split to get label distributions
for fold, (_, val_idx) in enumerate(skf.split(file_names, types)):
    val_labels = [types[i] for i in val_idx]
    for label in val_labels:
        fold_label_counts[f"fold_{fold}"][label] += 1

# Convert to DataFrame
strat_df = pd.DataFrame(fold_label_counts).fillna(0).astype(int)

# Calculate percentage per fold
strat_percent_df = strat_df.div(total_label_counts, axis=0).round(3) * 100

print("📊 Stratification percentage per class per fold:")
print(strat_percent_df)
#
#                                         fold_0  fold_1  fold_2  fold_3  fold_4
#Setariaviridis_large_A10svra3_33DAS        20.5    19.3    19.3    20.5    20.5
#Setariaviridis_medium_A10svra3_31DAS       19.4    20.9    20.9    19.4    19.4
#Setariaviridis_nonesmall_A10svra3_21DAS    19.4    20.8    20.8    19.4    19.4
#Setariaviridis_xlarge_A10svra3_42DAS       20.7    19.0    19.0    20.7    20.7
#Zeamays_large_tb1gt1ra3hybrid_anthesis     20.0    20.0    20.0    20.0    20.0
#Zeamays_medium_gt1ra3_anthesis             20.5    19.9    19.9    19.9    19.9
#Zeamays_none_B73_anthesis                  20.6    17.6    20.6    20.6    20.6
#Zeamays_nonesmall_gt1P_anthesis            19.8    21.0    19.8    19.8    19.8

# % class distribution *within* each fold
fold_class_ratios = strat_df.div(strat_df.sum(axis=0), axis=1).round(3) * 100

print("📊 Class composition within each fold (% of fold total):")
print(fold_class_ratios)

#                                         fold_0  fold_1  fold_2  fold_3  fold_4
#Zeamays_medium_gt1ra3_anthesis             26.1    25.4    25.4    25.4    25.4
#Zeamays_large_tb1gt1ra3hybrid_anthesis      6.7     6.8     6.8     6.8     6.8
#Setariaviridis_medium_A10svra3_31DAS       10.9    11.9    11.9    11.0    11.0
#Setariaviridis_large_A10svra3_33DAS        15.1    14.4    14.4    15.3    15.3
#Setariaviridis_nonesmall_A10svra3_21DAS    11.8    12.7    12.7    11.9    11.9
#Zeamays_none_B73_anthesis                   5.9     5.1     5.9     5.9     5.9
#Zeamays_nonesmall_gt1P_anthesis            13.4    14.4    13.6    13.6    13.6
#Setariaviridis_xlarge_A10svra3_42DAS       10.1     9.3     9.3    10.2    10.2


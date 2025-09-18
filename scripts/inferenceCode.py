#set up
import torch, detectron2
from detectron2.utils.logger import setup_logger
import numpy as np
import os, json, cv2, random
from detectron2 import model_zoo
from detectron2.engine import DefaultPredictor
from detectron2.config import get_cfg
from detectron2.utils.visualizer import Visualizer
from detectron2.data import MetadataCatalog, DatasetCatalog
from matplotlib import pyplot as plt
from detectron2.utils.visualizer import ColorMode
import pandas as pd

%matplotlib inline
cfg = get_cfg()

#using mask-rcnn config, setting weight to model 0001399.pth
cfg.merge_from_file(model_zoo.get_config_file("COCO-InstanceSegmentation/mask_rcnn_R_101_FPN_3x.yaml"))
cfg.MODEL.DEVICE = 'cpu'
cfg.MODEL.ROI_HEADS.NUM_CLASSES = 1

#maize setaria joint model
cfg.MODEL.WEIGHTS = "/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/training_maize_setaria_joint/output/model_0001599.pth"
#set confidence threshold
cfg.MODEL.ROI_HEADS.SCORE_THRESH_TEST = 0.8
predictor = DefaultPredictor(cfg)

#functions for running analysis

#output data should look like
#image_id, planting_date, genotype, treatment, pistil_area, pistil_circularity, bbox, segm(RLE encoding), confidence
def inferenceOnData(files,planting_date,genotype,treatment,savePath):
        def buildInferenceTable(image_id, outputs):
            #generate temp dataframe for individual image
            df = pd.DataFrame(columns=['image_id', 'planting_date', 'genotype', 'treatment', 'pistil_area', 'pistil_circularity', 'bbox', 'segm', 'confidence'])
            if len(outputs["instances"])==0:
                print("No detections for image")
                return df
            for i in range(0,len(outputs["instances"])):
                pred = outputs["instances"][i]
                pmask = pred.pred_masks.numpy().astype(int)[0]
                df.loc[len(df.index)] = [str(image_id), 
                                         str(planting_date),
                                         str(genotype), 
                                         str(treatment),
                                         calcAreaOfBinaryMask(pmask),
                                         calcCircularity(pmask),
                                         pred.pred_boxes.tensor.numpy(),
                                         str(binaryMaskToRLE(pmask.astype(np.uint8))),
                                         pred.scores.numpy()[0]]
            return df

        all_dfs = pd.DataFrame()
        for f in files:
            im = cv2.imread(f)
            #run inference
            outputs = predictor(im)
            #send output to analysis
            all_dfs = pd.concat([all_dfs, buildInferenceTable(f, outputs)])
            print("Progress:", files.index(f), "/", len(files), "Running inference on", f)
            #print(all_dfs) for debug
        all_dfs.to_csv(savePath + ".csv")
        return all_dfs

from pycocotools import mask
#assuming mask is [0,1] format
def calcAreaOfBinaryMask(pmask):
    return np.sum(pmask)

def calcPerimeterofBinaryMask(pmask):
    contours, _ = cv2.findContours(pmask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    perimeter = cv2.arcLength(contours[0], True)
    return perimeter
    
def calcCircularity(pmask):
    pmask = pmask.astype(np.uint8)
    perimeter = calcPerimeterofBinaryMask(pmask)
    area = calcAreaOfBinaryMask(pmask)
    circularity = ((4 * np.pi * area)) / (perimeter * perimeter)
    return circularity

def binaryMaskToRLE(pmask):
    return mask.encode(np.asfortranarray(pmask))


#specify dataset (using 21UM data)

planting_date="late_planting" #early or late
genotype="gt1ra3tb1"
#treatment="no_treatment" #or defoliated
directory="/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/maize_21UM_data/" + planting_date + "/" + genotype #+ "/"+ treatment
print(directory)
print(planting_date, genotype, treatment)
all_items = os.listdir(directory)
print("Total images, all angles:", len(all_items))
#filter images for angle 08
files08 = [os.path.join(directory, item) for item in all_items if item.endswith('08.jpg') and os.path.isfile(os.path.join(directory, item))]
print("Images angle 08:", len(files08))

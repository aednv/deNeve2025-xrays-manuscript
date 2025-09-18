#functions for calculating mask IoU

#set up
import numpy as np
from pycocotools import mask
import cv2

#converts polygon to binary mask, input is a single polygon mask from Detectron2
def toBinaryMask(seg):
    height, width = 672, 1008  # dimensions of all images (modify depending on dataset)
    maskk = np.zeros([height, width], dtype=np.uint8)
    polygon = np.array(seg).reshape(-1, 2).astype(int)
    cv2.fillPoly(maskk, [polygon], 1)
    gt_mask = np.array(maskk).astype(np.uint8)
    return gt_mask

#calculates individual mask IoU for each image in the test set, as well as the mean IoU per image. Input is the test set datasetDict from Detectron2.
#uses coco api for IoU calc
def calcTestsetMaskIoU(test_dataset):
    datasetDict=test_dataset
    #iterate through each image in test set
    meanIous = []
    for d in datasetDict:
        topIous = []
        print("Calculating IoU for image", d['image_id']+1, "of", len(datasetDict), d['file_name'])
        #run inference on image
        im = cv2.imread(d["file_name"])
        outputs = predictor(im)
        #iterate through each prediction in image 
        for pred in outputs['instances'].pred_masks:
            #convert mask to binary, rle encoding
            pred_mask = np.array(pred).astype(np.uint8)
            rle_pred = mask.encode(np.asfortranarray(pred_mask))
            #iterate through each ground truth mask for image
            currTop=0
            for annos in d["annotations"]:
                current_seg = annos["segmentation"][0]
                binary_gt_mask = toBinaryMask(current_seg)
                rle_gt = mask.encode(np.asfortranarray(binary_gt_mask))
                currIou = mask.iou([rle_gt], [rle_pred], [0])[0, 0]
                #print(currIou)
                #print(currTop)
                if currIou > currTop:
                    currTop=currIou
            topIous.append(currTop)
        print("Individual mask IoUs", topIous)
        print("Mean IoU for image", np.mean(topIous))
        meanIous.append(np.mean(topIous))
    print("Mean IoUs for each image in test set", meanIous)
    print("Mean across whole test set", np.mean(meanIous))

#train the grass pistil segmentaion models using mask-rcnn (detectron2)

#set up
import torch, detectron2
print("detectron2:", detectron2.__version__)
from detectron2.utils.logger import setup_logger
setup_logger()

# import common libraries
import numpy as np
import os, json, cv2, random

# import detectron2 utilities
from detectron2 import model_zoo
from detectron2.engine import DefaultPredictor
from detectron2.config import get_cfg
from detectron2.utils.visualizer import Visualizer
from detectron2.data import MetadataCatalog, DatasetCatalog
from matplotlib import pyplot as plt
from detectron2.evaluation import COCOEvaluator

#import annotation data (coco-json format using via annotator)
from detectron2.data.datasets import register_coco_instances

#register all data
base_dir = "/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/stratifiedKfoldGen/annotations/fold_4"

register_coco_instances("fold4_train", {}, os.path.join(base_dir, "train.json"), "/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/annotations/stratifiedKfolds")

register_coco_instances("fold4_val", {}, os.path.join(base_dir, "val.json"), "/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/annotations/stratifiedKfolds")



#train mask-rcnn model
from detectron2.engine import DefaultTrainer

cfg = get_cfg()
cfg.merge_from_file(model_zoo.get_config_file("COCO-InstanceSegmentation/mask_rcnn_R_101_FPN_3x.yaml"))
cfg.DATASETS.TRAIN = ("fold4_train",)
cfg.DATASETS.TEST = ("fold4_val",)
cfg.MODEL.DEVICE = 'cpu'
cfg.DATALOADER.NUM_WORKERS = 1

cfg.MODEL.WEIGHTS = model_zoo.get_checkpoint_url("COCO-InstanceSegmentation/mask_rcnn_R_101_FPN_3x.yaml") #initialize from model zoo
cfg.SOLVER.IMS_PER_BATCH = 2
cfg.SOLVER.BASE_LR = 0.00025
cfg.SOLVER.MAX_ITER = 2000    # 2000 iterations max
cfg.SOLVER.STEPS = []        # do not decay learning rate
cfg.SOLVER.CHECKPOINT_PERIOD = 200
cfg.MODEL.ROI_HEADS.BATCH_SIZE_PER_IMAGE = 128
cfg.MODEL.ROI_HEADS.NUM_CLASSES = 1
cfg.TEST.EVAL_PERIOD = 100

from MyTrainer import MyTrainer
from LossEvalHook import LossEvalHook

cfg.OUTPUT_DIR = "/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/training_stratifiedKfold4/output"

os.makedirs(cfg.OUTPUT_DIR, exist_ok=False)
trainer = MyTrainer(cfg) 
trainer.resume_or_load(resume=False)

trainer.train()

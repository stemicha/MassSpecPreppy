## import depedencies
## install in R
## library(reticulate)
## py_install(packages = "opentrons",pip = T)
## dependencies
import json
from opentrons import protocol_api
from opentrons.types import Point
import csv
import os
import math

# values insert
def get_values(*names):
    import json
    _all_values = json.loads("""{"csv_sample":"sample,protein concentration (µg/µl),OT-2 slot,OT-2 position,volume,preparation plate position\\nSample_1,2,samples1,A1,100,A1\\nSample_2,2,samples1,B1,100,B1\\nSample_3,1,samples1,C1,15,C1\\nSample_4,1,samples1,D1,100,D1\\nSample_5,1,samples1,A2,100,E1\\nSample_6,1,samples1,B2,100,F1\\nSample_7,1,samples1,C2,100,G1\\nSample_8,1,samples1,D2,100,H1\\nSample_9,1,samples1,A3,100,A2\\nSample_10,1,samples1,B3,100,B2\\nSample_11,1,samples1,C3,100,C2\\nSample_12,1,samples1,D3,100,D2\\nSample_13,1,samples1,A4,100,E2","sample_amount":"4","trypsin_ratio":"50","trypsin_stock_concentration":"40","EvoTips_amount_ng":"500"}""")
    return [_all_values[n] for n in names]



#specify meta data
metadata = {
     "protocolName": "Mass Spec Preppy: step 4 > post EvoTip loading elution of acidified peptide mixture",
     "apiLevel": "2.12",
     "description": "This part of the Mass Spec Preppy workflow will perform the elution of acidified peptide mixture after EvoTip loading",
     "author": "Stephan Michalik <stephan.michalik@uni-greifswald.de>"}


#run protocol from here on
#remember python counts from 0 in iteration and postion in lists etc.!!!!
def run(protocol: protocol_api.ProtocolContext):
  
  #get csv values from JSON above
  [csv_sample, sample_amount, trypsin_ratio, trypsin_stock_concentration,LysC_ratio,LysC_stock_concentration,EvoTips_amount_ng,LysC_Trypsin_Mix_stock_concentration,LysC_Trypsin_Mix_ratio] = get_values("csv_sample","sample_amount","trypsin_ratio", "trypsin_stock_concentration","LysC_ratio","LysC_stock_concentration","EvoTips_amount_ng","LysC_Trypsin_Mix_stock_concentration","LysC_Trypsin_Mix_ratio")

  #convert to float number
  sample_amount = float(sample_amount)
  trypsin_ratio = float(trypsin_ratio)
  LysC_ratio = float(LysC_ratio)
  trypsin_stock_concentration = float(trypsin_stock_concentration)
  LysC_stock_concentration = float(LysC_stock_concentration)
  LysC_Trypsin_Mix_stock_concentration = float(LysC_Trypsin_Mix_stock_concentration)
  LysC_Trypsin_Mix_ratio = float(LysC_Trypsin_Mix_ratio)
  EvoTips_amount_ng = float(EvoTips_amount_ng)

  #calculate sample loading dilution for EvoTip / setup 23µl take out 20 µl / 15% more
  EvoTips_amount_ng_volume = ((EvoTips_amount_ng/20)/((sample_amount*1000)/24.44))*23
  EvoTips_amount_ng_volume_buffer = 23-EvoTips_amount_ng_volume
  
  # csv_sample --> nested list
  SampleTransfer = [[val.strip() for val in line.split(",")]
             for line in csv_sample.splitlines()
             if line.split(",")[0].strip()][1:]
  # sample numbers
  sample_number = len(SampleTransfer)
  #column of sample for multichannel
  num_cols = math.ceil(sample_number/8)

  #paramters for magnet
  OFFSET_RADIAL = 1.5  # in mm
  OFFSET_Z = 3.0  # in mm
  OFFSET_Z_remove = 0.6 # in mm
  MAGNET_HEIGHT = 12.0  # in mm
  INCUBATION_TIME = 5  # in minutes
  
  #load magnetic module
  magnet_module = protocol.load_module("magnetic module gen2", "1")
  #preparation plate on magnet
  magplate = magnet_module.load_labware("nest_96_wellplate_100ul_pcr_full_skirt")
  mag_samples = magplate.rows()[0][:num_cols]

  #prep plate column positions
  prep_plate_columns_position = ["A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"]

 
  #dilution plate
  elution_plate = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "2",label = "elution_plate")

  #setup labware
  #tips
  tiprack20_1 = protocol.load_labware("opentrons_96_tiprack_20ul", "3",label = "OT_96_tiprack_20ul")

  #pipettes
  m20 = protocol.load_instrument("p20_multi_gen2", mount = "right", tip_racks = [tiprack20_1])


  ##################################################
  # transfer sample to dilution plate
  ##################################################
  #engage magnet
  magnet_module.engage(height=MAGNET_HEIGHT)
  protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')

  m20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
  for i in range(0,  math.ceil(sample_number/8)):
    side = -1 if i % 2 == 0 else 1
    loc = magplate.wells_by_name()[prep_plate_columns_position[i]].bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z_remove))
    m20.transfer(25,
                 loc,
                 elution_plate.wells_by_name()[prep_plate_columns_position[i]].bottom(1),
                 new_tip="once")
  m20.flow_rate.aspirate *= 10  # modulate flow rate for removal

  #comment step
  protocol.comment("take elution plate out >> store it ! DONE!")
  
  #disengage magnet module         
  magnet_module.disengage()

## import depedencies
## install in R
## library(reticulate)
## py_install(packages = "opentrons",pip = T)
## dependencies
import json
from opentrons.types import Location, Point
from opentrons import protocol_api
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
     "protocolName": "Mass Spec Preppy: step 3 > EvoTip loading/peptide purification",
     "apiLevel": "2.12",
     "description": "This part of the Mass Spec Preppy workflow will perform the elution/loading of EvoTips",
     "author": "Stephan Michalik <stephan.michalik@uni-greifswald.de>"}


#run protocol from here on
#remember python counts from 0 in iteration and postion in lists etc.!!!!
def run(protocol: protocol_api.ProtocolContext):
  # input x, y, z values here (in mm). 
  # The coordinates are absolute 
  # with reference to the bottom left corner of slot 1 as origin.
  # x, y, and z can be a float or integer
  locPause = Location(Point(40, 340, 150), None) # location for slot 10
  
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

  #calculate sample loading dilution for EvoTip / setup 23µl take out 20 µl / 15% more BUT use at least 1 µl
  if 1/((EvoTips_amount_ng/20)/((sample_amount*1000)/24.44)) > 23:
    EvoTips_amount_ng_volume = 1
    EvoTips_amount_ng_volume_buffer = 1/((EvoTips_amount_ng/20)/((sample_amount*1000)/24.44))-EvoTips_amount_ng_volume
  else:
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
  INCUBATION_TIME = 3 # in minutes
  
  #load magnetic module
  magnet_module = protocol.load_module("magnetic module gen2", "1")
  #preparation plate on magnet
  magplate = magnet_module.load_labware("nest_96_wellplate_100ul_pcr_full_skirt")
  mag_samples = magplate.rows()[0][:num_cols]

  #prep plate column positions
  prep_plate_columns_position = ["A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"]

  #reagent plate
  #column 1 = 2 ml 5%TFA max. 0.23ml for first step used - acidification
  #column 2 = solvent B: 0.1% formic acid/95% ACN for initial tip washing step
  #column 3 = solvent A: 0.1% formic acid - 2x wash
  #column 4 = solvent A: 0.1% formic acid - storage
  reagents = protocol.load_labware("nest_12_reservoir_15ml", "2",label = "reagents")
  
  #dilution plate
  dilution_plate = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "5",label = "dilution_plate")

  #vial labware (48 samples per slot)
  EvoTips = protocol.load_labware("cfungeneapapterevotips_96_tuberack_200ul", "3",label = "EvoTips")


  #setup labware
  #tips
  tiprack300_1 = protocol.load_labware("opentrons_96_tiprack_300ul", "9",label = "OT_96_tiprack_300ul_1")
 
  tiprack20_1 = protocol.load_labware("opentrons_96_tiprack_20ul", "10",label = "OT_96_tiprack_20ul_1")
  tiprack20_2 = protocol.load_labware("opentrons_96_tiprack_20ul", "7",label = "OT_96_tiprack_20ul_2")
  tiprack20_3 = protocol.load_labware("opentrons_96_tiprack_20ul", "11",label = "OT_96_tiprack_20ul_3")
  tiprack20_4 = protocol.load_labware("opentrons_96_tiprack_20ul", "8",label = "OT_96_tiprack_20ul_4")


  #pipettes
  m20 = protocol.load_instrument("p20_multi_gen2", mount = "right", tip_racks = [tiprack20_1,tiprack20_2,tiprack20_3,tiprack20_4])
  m300 = protocol.load_instrument("p300_multi_gen2", mount = "left", tip_racks = [tiprack300_1])
 
  #volume from previous step = 22µl
  
  ##################################################
  # add 5% TFA to a final conc. of 0.5% TFA
  ##################################################
  #engage magnet
  magnet_module.engage(height=MAGNET_HEIGHT)
  protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')

  for i in range(0,  math.ceil(sample_number/8)):
    m20.transfer(2.44, 
                 reagents.wells_by_name()["A1"].bottom(1), # 1mm from bottom
                 magplate.wells_by_name()[prep_plate_columns_position[i]].bottom(1), 
                 new_tip ="always",
                 blow_out = True,
                 blowout_location = "destination well",
                 mix_after = (2,10),
                 touch_tip = False)
  # vol = 24.44 ul
  
  ##################################################
  # transfer dilution buffer for 20µl sample dilution
  ##################################################
  m20.pick_up_tip()
  for i in range(0,  math.ceil(sample_number/8)):
    m20.transfer(EvoTips_amount_ng_volume_buffer, 
                 reagents.wells_by_name()["A3"].bottom(1), # 1mm from bottom
                 dilution_plate.wells_by_name()[prep_plate_columns_position[i]].bottom(1), 
                 new_tip ="never",
                 touch_tip = False)
  m20.drop_tip()


  ##################################################
  # transfer sample to dilution plate
  ##################################################

  m20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
  for i in range(0,  math.ceil(sample_number/8)):
    side = -1 if i % 2 == 0 else 1
    loc = magplate.wells_by_name()[prep_plate_columns_position[i]].bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z_remove))
    m20.transfer(EvoTips_amount_ng_volume,
                 loc,
                 dilution_plate.wells_by_name()[prep_plate_columns_position[i]].bottom(1),
                 new_tip="always", 
                 mix_after = (2,10))
  m20.flow_rate.aspirate *= 10  # modulate flow rate for removal

  
  ######################################################
  # add solvent B to EvoTip / / washing step of pure tip
  ######################################################
  m300.pick_up_tip()
  m300.distribute(20, 
                 reagents.wells_by_name()["A2"].bottom(1), # 1mm from bottom
                 [EvoTips.wells_by_name()[well_name].bottom().move(Point(x=-1, z=35)) for well_name in prep_plate_columns_position[0:num_cols]],#35 mm above
                 new_tip = "never",
                 mix_before = (1,200),
                 air_gap = 20,
                 touch_tip = False)
  m300.drop_tip()
  
  #pause step
  m300.move_to(locPause)
  protocol.pause("take EvoTips out >> centrifuge!!! & soak tips afterwards in Propanol and resume")

  #####################################################
  # add solvent A to EvoTip / washing step of pure tip
  #####################################################
  m300.pick_up_tip()
  m300.distribute(20, 
                 reagents.wells_by_name()["A3"].bottom(1), # 1mm from bottom
                 [EvoTips.wells_by_name()[well_name].bottom().move(Point(x=-1, z=35)) for well_name in prep_plate_columns_position[0:num_cols]],#35 mm above
                 new_tip ="never",
                 touch_tip = False)
  
  m300.drop_tip()
  #pause step
  m300.move_to(locPause)
  protocol.pause("take EvoTips out >> centrifuge!!! and resume")


  ##################################################
  # transfer samples to Evotip /sample loading
  ##################################################
  for i in range(0, math.ceil(sample_number/8)):
      m20.transfer(20, 
                 dilution_plate.wells_by_name()[prep_plate_columns_position[i]].bottom(0.5), # 0.5mm from bottom
                 EvoTips.wells_by_name()[prep_plate_columns_position[i]].bottom(15),#15 mm above
                 new_tip ="always",
                 touch_tip = False)
 
  #pause step
  m300.move_to(locPause)
  protocol.pause("take EvoTips out >> centrifuge!!! and resume")


  ##################################################
  # add solvent A to EvoTip / washing step
  ##################################################
  m300.pick_up_tip()
  m300.distribute(20, 
                 reagents.wells_by_name()["A3"].bottom(1), # 1mm from bottom
                 [EvoTips.wells_by_name()[well_name].bottom().move(Point(x=-1, z=35)) for well_name in prep_plate_columns_position[0:num_cols]],#35 mm above
                 new_tip ="never",
                 touch_tip = False)
  m300.drop_tip()
  #pause step
  m300.move_to(locPause)
  protocol.pause("take EvoTips out >> centrifuge!!! and resume")

  ##################################################
  # add solvent A to EvoTip / storage step
  ##################################################
  m300.distribute(100, 
                 reagents.wells_by_name()["A4"].bottom(1), # 1mm from bottom
                 [EvoTips.wells_by_name()[well_name].bottom().move(Point(x=-1, z=35)) for well_name in prep_plate_columns_position[0:num_cols]],#35 mm above
                 new_tip ="always",
                 blow_out = True,
                 blowout_location = "source well",
                 touch_tip = False)
  #comment step
  protocol.comment("take EvoTips out >> centrifuge!!! keep them wet ! DONE!")
  
  #disengage magnet module         
  magnet_module.disengage()

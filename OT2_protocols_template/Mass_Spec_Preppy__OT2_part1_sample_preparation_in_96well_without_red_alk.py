## SP3 - Mass Spec Preppy - part1 - sample preparation without reduction and alkylation 
##########################################################################################################################################################

# dependencies
import json
from opentrons import protocol_api
import csv
import os
import math
import contextlib
import threading

# values insert
def get_values(*names):
    import json
    _all_values = json.loads("""{"csv_sample":"sample,protein concentration (µg/µl),OT-2 slot,OT-2 position,volume,preparation plate position\\ntest_1,1.5,samples1,A1,100,A1\\nTest_2,1.34,samples4,B1,100,B1","sample_amount":"4","trypsin_ratio":"100","trypsin_ratio":"100","trypsin_stock_concentration":"20"}""")
    return [_all_values[n] for n in names]



#specify meta data
metadata = {
     "protocolName": "Mass Spec Preppy: step 1 > sample preparation in 96well format WITHOUT reduction/alkylation & add SP3 beads",
     "apiLevel": "2.12",
     "description": "This part of the Mass Spec Preppy workflow will prepare the specified protein amount of the samples in a 96well plate WITHOUT reduction/alkylation and SP3 bead addition",
     "author": "Stephan Michalik <stephan.michalik@uni-greifswald.de>"}

#liquid heights
min_height = 0.5  # depth at which the pipette tip will stop descending into the tube

#function for height offset of 1.5ml tubes
def vol_height_1_5_ml(initial_volume, aspirate_volume, height_below_mm):
  # calculate height based on volume (measurement with water and caliper); fit using log2 scaled volume and polynomial fit
  vol = initial_volume - aspirate_volume
  dh = 8.277950e-02*(vol)-2.103135e-04*(math.pow(vol,2))+3.147543e-07*(math.pow(vol,3))-2.185521e-10*(math.pow(vol,4))+5.604949e-14*(math.pow(vol,5))+1.065421e+00
  #substract height_below_mm from calculated height 
  height_out = dh - height_below_mm
  # make sure height decrement will not crash into the bottom of the tube
  if height_out < min_height:  height_out = min_height
  #return height
  return height_out


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

  
  # csv_sample --> nested list
  SampleTransfer = [[val.strip() for val in line.split(",")]
             for line in csv_sample.splitlines()
             if line.split(",")[0].strip()][1:]
  
  #sample and buffer volumes for dynamic heights
  sample_end_volume = 15.625
  sample_number = len(SampleTransfer)

  #apend sample and dilution volume postion to nested list
  for i in range(len(SampleTransfer)):
      #sample volume
      SampleTransfer[i].append(sample_amount/float(SampleTransfer[i][1]))
      #buffer volume
      SampleTransfer[i].append(sample_end_volume-(sample_amount/float(SampleTransfer[i][1])))

   
  #setup labware
  #tips
  tiprack20_1 = protocol.load_labware("opentrons_96_tiprack_20ul", "10",label = "OT_96_tiprack_20ul_1")
  tiprack20_2 = protocol.load_labware("opentrons_96_tiprack_20ul", "7",label = "OT_96_tiprack_20ul_2")
  tiprack20_3 = protocol.load_labware("opentrons_96_tiprack_20ul", "11",label = "OT_96_tiprack_20ul_3")
  
  #preparation plate
  prep_plate = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "6", label = "preparation_plate")
  
  #pipettes
  p20 = protocol.load_instrument("p20_single_gen2", mount = "left", tip_racks = [tiprack20_1,tiprack20_2])
  m20 = protocol.load_instrument("p20_multi_gen2", mount = "right", tip_racks = [tiprack20_3])

  #reagent plate
  #column 1 = 150µl buffer in each well
  #column 2 = 150µl buffer in each well
  #column 3 = xxx SP3 in each well
  reagents = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "3",label = "reagents")
  
  #samples
  samples1 = protocol.load_labware("opentrons_24_tuberack_eppendorf_1.5ml_safelock_snapcap", "4",label = "samples_1")
  samples2 = protocol.load_labware("opentrons_24_tuberack_eppendorf_1.5ml_safelock_snapcap", "1",label = "samples_2")
  samples3 = protocol.load_labware("opentrons_24_tuberack_eppendorf_1.5ml_safelock_snapcap", "5",label = "samples_3")
  samples4 = protocol.load_labware("opentrons_24_tuberack_eppendorf_1.5ml_safelock_snapcap", "2",label = "samples_4")
  
  #POSITIONING
  #generate buffer positions for single pipette p20
  buffer_postion = ["A1","B1","C1","D1","E1","F1","G1","H1","A2","B2","C2","D2","E2","F2","G2","H2"]
  buffer_postion_idx = 0
  buffer_volume = 150
  
  #prep plate column positions
  prep_plate_columns_position = ["A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"]

  
  #transfer buffer
  p20.pick_up_tip()
  for i in range(0,len(SampleTransfer)):
    if SampleTransfer[i][2] == "samples1": p20.transfer(SampleTransfer[i][7], 
                                                    reagents.wells_by_name()[buffer_postion[buffer_postion_idx]],
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="never", 
                                                    touch_tip = False)
    if SampleTransfer[i][2] == "samples2": p20.transfer(SampleTransfer[i][7], 
                                                    reagents.wells_by_name()[buffer_postion[buffer_postion_idx]],
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="never", 
                                                    touch_tip = False)
    if SampleTransfer[i][2] == "samples3": p20.transfer(SampleTransfer[i][7], 
                                                    reagents.wells_by_name()[buffer_postion[buffer_postion_idx]],
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="never", 
                                                    touch_tip = False)
    if SampleTransfer[i][2] == "samples4": p20.transfer(SampleTransfer[i][7], 
                                                    reagents.wells_by_name()[buffer_postion[buffer_postion_idx]],
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="never", 
                                                    touch_tip = False)
    #set buffer volume
    buffer_volume = buffer_volume-SampleTransfer[i][7]
    #switch to next well
    if buffer_volume<20:
      buffer_postion_idx = buffer_postion_idx+1
      buffer_volume =150

  #drop tip after buffer
  p20.drop_tip()

                                                    
  #transfer samples                                                 
  for i in range(0,len(SampleTransfer)):
    if SampleTransfer[i][2] == "samples1": p20.transfer(SampleTransfer[i][6], 
                                                    samples1.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=6)),
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="always",
                                                    mix_after = (3,7),
                                                    touch_tip = False)
    if SampleTransfer[i][2] == "samples2": p20.transfer(SampleTransfer[i][6], 
                                                    samples2.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=6)),
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="always", 
                                                    mix_after = (3,7),
                                                    touch_tip = False)
    if SampleTransfer[i][2] == "samples3": p20.transfer(SampleTransfer[i][6], 
                                                    samples3.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=6)),
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="always",
                                                    mix_after = (3,7),
                                                    touch_tip = False)
    if SampleTransfer[i][2] == "samples4": p20.transfer(SampleTransfer[i][6], 
                                                    samples4.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=6)),
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]],
                                                    new_tip ="always",
                                                    mix_after = (3,7),
                                                    touch_tip = False)
    
  

  #pcr plate loading beads 
  #60µl loading to plate 
  #adding SP3 beads 5 µl of 50µg/µl ready to use beads
  m20.flow_rate.aspirate = 20
  m20.flow_rate.dispense = 20
  for i in range(0, math.ceil(sample_number/8)):
    m20.transfer(5, 
                 reagents.wells_by_name()["A3"].bottom(3), # 3mm from bottom
                 prep_plate.wells_by_name()[prep_plate_columns_position[i]].bottom(1),
                 new_tip ="always",
                 mix_before = (12,20),
                 touch_tip = False)
  #vol = 20.625µl

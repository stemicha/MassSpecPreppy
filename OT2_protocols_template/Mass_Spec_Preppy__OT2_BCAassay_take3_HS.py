#import depedencies
## install in R
## library(reticulate)
## py_install(packages = "opentrons",pip = T)

# dependencies
import json
from opentrons import protocol_api
import csv
import os
import math

# values insert
def get_values(*names):
    import json
    _all_values = json.loads("""{"csv_sample":"Sample,Dilution,OT2_samples,OT-2 position,sample_volume\\n500ul_sample,10,samples1,A1,500\\n10ul_sample,50,samples1,B1,10\\n500ul_sample,50,samples1,C1,500\\n10ul_sample,50,samples1,D1,500\\n500ul_sample,10,samples1,A2,500\\n10ul_sample,10,samples1,B2,500\\n500ul_sample,10,samples1,C2,500\\n10ul_sample,50,samples1,D2,500\\n500ul_sample,50,samples1,A3,500\\n10ul_sample,10,samples1,B3,500\\n500ul_sample,50,samples1,C3,500\\n10ul_sample,50,samples1,D3,500\\n500ul_sample,10,samples1,A4,500\\n10ul_sample,10,samples1,B4,500\\n500ul_sample,10,samples1,C4,500\\n10ul_sample,50,samples1,D4,500\\n500ul_sample,50,samples1,A5,500\\n10ul_sample,10,samples1,B5,500\\n500ul_sample,10,samples1,C5,500\\n10ul_sample,10,samples1,D5,500\\n500ul_sample,10,samples1,A6,500\\n10ul_sample,10,samples1,B6,500\\n500ul_sample,10,samples1,C6,500\\n10ul_sample,10,samples1,D6,500\\n500ul_sample,10,samples2,A1,500\\n10ul_sample,10,samples2,B1,500\\n500ul_sample,10,samples2,C1,500\\n10ul_sample,10,samples2,D1,500\\n500ul_sample,10,samples2,A2,500\\n10ul_sample,10,samples2,B2,500\\n500ul_sample,10,samples2,C2,500\\n10ul_sample,10,samples2,D2,500\\n500ul_sample,10,samples2,A3,500\\n10ul_sample,10,samples2,B3,500\\n500ul_sample,10,samples2,C3,500\\n10ul_sample,10,samples2,D3,500\\n500ul_sample,10,samples2,A4,500\\n10ul_sample,10,samples2,B4,500\\n500ul_sample,10,samples2,C4,500\\n10ul_sample,10,samples2,D4,500"}""")
    return [_all_values[n] for n in names]



#specify meta data
metadata = {
     "protocolName": "BCA protein concentration determination with OT-2 with CSV File",
     "apiLevel": "2.13",
     "description": "BCA assay for take3 plate",
     "author": "Stephan Michalik <stephan.michalik@uni-greifswald.de>"}

#liquid heights
min_height = 1  # depth at which the pipette tip will stop descending into the tube

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
  
  #csv values from JSON above
  [csv_sample] = get_values("csv_sample")
  # csv file --> nested list
  SampleTransfer = [[val.strip() for val in line.split(",")]
             for line in csv_sample.splitlines()
             if line.split(",")[0].strip()][1:]
                  
  #define prep plate sample postion
  samples_plate_position = ["A2","B2","C2","D2","E2","F2","G2","H2",
                            "A3","B3","C3","D3","E3","F3","G3","H3",
                            "A4","B4","C4","D4","E4","F4","G4","H4",
                            "A5","B5","C5","D5","E5","F5","G5","H5",
                            "A6","B6","C6","D6","E6","F6","G6","H6"]
  
  sample_dilution_end_volume = 50
  standard_dilution_end_volume = 50
  standard_initial_volume = 400
  sample_number = len(SampleTransfer)
  #min. sample dilutuion end-volume [µl]
  min_vol_sample_dilution = 25
  #min. max. sample volume [µl] for transfer
  min_sample_volume_transfer = 1

  #append sample and dilution volume postion to nested list
  for i in range(len(SampleTransfer)):
      #sample position
      SampleTransfer[i].append(samples_plate_position[i])
      #sample end volume calculation
      if min_sample_volume_transfer*float(SampleTransfer[i][1])<min_vol_sample_dilution :
        # sample volume needed
        SampleTransfer[i].append(min_vol_sample_dilution/float(SampleTransfer[i][1]))
        # sample end volume
        SampleTransfer[i].append(min_vol_sample_dilution)
      else :
        # sample volume needed
        SampleTransfer[i].append(min_sample_volume_transfer)
        # sample end volume
        SampleTransfer[i].append(min_sample_volume_transfer*float(SampleTransfer[i][1]))

  
  #modules
  hs_mod = protocol.load_module("heaterShakerModuleV1", "7")

  #tips
  tiprack20_1 = protocol.load_labware("opentrons_96_tiprack_20ul", "8",label = "opentrons_96_tiprack_20ul_1")
  tiprack20_2 = protocol.load_labware("opentrons_96_tiprack_20ul", "4",label = "opentrons_96_tiprack_20ul_2")
  tiprack20_3 = protocol.load_labware("opentrons_96_tiprack_20ul", "5",label = "opentrons_96_tiprack_20ul_3")
  tiprack20_4 = protocol.load_labware("opentrons_96_tiprack_20ul", "9",label = "opentrons_96_tiprack_20ul_4")

  #plates
  reagent_plate = protocol.load_labware("nest_12_reservoir_15ml", "2",label = "reagent_plate_nest_12_reservoir_15ml")
  #A1 = water min. 5ml
  #B1 = 1x sample buffer min. 10ml
  #C1 = 2x sample buffer min. 5ml
  #D1 = BCA working reagent min . 10ml
  BCA_plate = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "1", label = "BCA_PCR_plate")
  prep_plate = hs_mod.load_labware("opentrons_96_pcr_adapter_nest_wellplate_100ul_pcr_full_skirt", label  = "preparation_PCR_plate")

  #samples
  samples1 = protocol.load_labware("opentrons_24_tuberack_eppendorf_1.5ml_safelock_snapcap", "6",label = "samples1")
  samples2 = protocol.load_labware("opentrons_24_tuberack_eppendorf_1.5ml_safelock_snapcap", "3",label = "samples2")
  # D6 samples 2 = BSA stock 1mg/ml min. 350µl
  
  #pipettes
  p20 = protocol.load_instrument("p20_single_gen2", mount = "left", tip_racks = [tiprack20_1,tiprack20_2])
  m20 = protocol.load_instrument("p20_multi_gen2", mount = "right", tip_racks=[tiprack20_3,tiprack20_4])

  #wells for transfer to plates with multichannel pipette
  prep_columns_positions = ["A2","A3","A4","A5","A6"]
  BCA_columns_position = [["A3","A4"],["A5","A6"],["A7","A8"],["A9","A10"],["A11","A12"]]
  WR_BCA_columns_position = [["A1","A2"],["A3","A4"],["A5","A6"],["A7","A8"],["A9","A10"],["A11","A12"]]

  #generate standard position and volumes
  std_positions = ["A1","B1","C1","D1","E1","F1","G1"]
  std_volumes = [16,12,8,6,4,2,1]

  # offset piptette over bottom: default 1mm
  bottom_offset = 1.5

  # close latch HS
  hs_mod.close_labware_latch()

  #std. dilution: transfer 2xBSA diluent
  m20.transfer(standard_dilution_end_volume,
              reagent_plate.wells_by_name()["A1"],
              prep_plate.wells_by_name()["A1"],
              touch_tip = True,
              new_tip = "always",
              disposal_volume = 0)
      

  #refine 2xBSA diluent with P20 / use one tip
  for i in range(0,7): p20.transfer(std_volumes[i], 
                                    prep_plate.wells_by_name()[std_positions[i]].bottom(bottom_offset),
                                    protocol.fixed_trash["A1"],
                                    touch_tip = True,
                                    new_tip = "always",
                                    disposal_volume = 0)
         
  #BSA standard dilution
  for i in range(0,7): 
    # print(vol_height_1_5_ml(initial_volume=standard_initial_volume,aspirate_volume=std_volumes[i],height_below_mm=3))
    p20.transfer(std_volumes[i], 
                samples2.wells_by_name()["D6"].bottom(vol_height_1_5_ml(initial_volume=standard_initial_volume,aspirate_volume=std_volumes[i],height_below_mm=5)),
                prep_plate.wells_by_name()[std_positions[i]].bottom(bottom_offset),
                new_tip ="always", 
                touch_tip = True,
                mix_after = (2, 20), 
                disposal_volume = 0)
                
    #standard height
    standard_initial_volume = standard_initial_volume - std_volumes[i]

  
  #dispense diluent for sample dilution
  m20.distribute(sample_dilution_end_volume,
                  reagent_plate.wells_by_name()["A2"],
                  [prep_plate.wells_by_name()[well_name].bottom(bottom_offset) for well_name in prep_columns_positions[: math.ceil(len(SampleTransfer)/8)]],
                  new_tip = "once",
                  touch_tip = True)
                  
  #refine sample diluent with P20 / use one tip
  for i in range(0,len(SampleTransfer)): p20.transfer((sample_dilution_end_volume-SampleTransfer[i][7])+SampleTransfer[i][6], 
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]].bottom(bottom_offset),
                                                    protocol.fixed_trash["A1"],
                                                    touch_tip = True,
                                                    new_tip = "always", 
                                                    disposal_volume = 0)    

   
  #sample dilution
  for i in range(0,len(SampleTransfer)):
    if SampleTransfer[i][2] == "samples1": p20.transfer(SampleTransfer[i][6], 
                                                    samples1.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=3)),
                                                    prep_plate.wells_by_name()[SampleTransfer[i][5]].bottom(bottom_offset),
                                                    new_tip ="always", 
                                                    touch_tip = True,
                                                    mix_after = (2, 20), 
                                                    disposal_volume = 0)
                
       
    else: p20.transfer(SampleTransfer[i][6], 
                      samples2.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=3)),
                      prep_plate.wells_by_name()[SampleTransfer[i][5]].bottom(bottom_offset),
                      new_tip ="always", 
                      touch_tip = True,
                      mix_after = (2, 20), 
                      disposal_volume = 0)
    
    #sample height adjustment
    for k in range(0,len(SampleTransfer)):
      if SampleTransfer[i][0]==SampleTransfer[k][0]: SampleTransfer[k][4] = float(SampleTransfer[k][4])-SampleTransfer[i][6]
  
  #shake plate
  hs_mod.set_and_wait_for_shake_speed(2000)
  protocol.delay(minutes=2)
  hs_mod.deactivate_shaker() 

  #distribute working solution to BCA plate
  m20.pick_up_tip()
  for i in range(0, math.ceil(sample_number/8)+1): m20.distribute(10,
                                                           reagent_plate.wells_by_name()["A4"],
                                                           [BCA_plate.wells_by_name()[well_name].bottom(bottom_offset) for well_name in WR_BCA_columns_position[i]],
                                                           new_tip = "never",
                                                           touch_tip = False,
                                                           blow_out = True,
                                                           blowout_location = "source well")
  m20.drop_tip()
                  
  #distribute 2x buffer to BCA plate
  m20.distribute(5,
                  reagent_plate.wells_by_name()["A3"],
                  [BCA_plate.wells_by_name()[well_name].bottom(bottom_offset) for well_name in ["A1","A2"]],
                  touch_tip = False,
                  new_tip = "once")
  
  #distribute 2x std. to BCA plate
  m20.distribute(5,
                  prep_plate.wells_by_name()["A1"].bottom(bottom_offset),
                  [BCA_plate.wells_by_name()[well_name].bottom(bottom_offset) for well_name in ["A1","A2"]],
                  new_tip = "once",
                  touch_tip = False,
                  mix_before = [2,20],
                  disposal_volume = 0)
  
  #distribute sample to BCA plate
  for i in range(0, math.ceil(sample_number/8)): m20.distribute(10,
                                                           prep_plate.wells_by_name()[prep_columns_positions[i]].bottom(bottom_offset),
                                                           [BCA_plate.wells_by_name()[well_name].bottom(bottom_offset) for well_name in BCA_columns_position[i]],
                                                           new_tip = "once",
                                                           touch_tip = False,
                                                           mix_before = [2,20],
                                                           disposal_volume = 0)
 
  # open latch HS
  hs_mod.open_labware_latch()

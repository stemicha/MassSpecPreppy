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
    _all_values = json.loads("""{"csv_sample":"Sample,Dilution,OT2_samples,OT-2 position,sample_volume\\nsample_5x,5,samples1,A1,30\\nsample_10x,10,samples1,B1,30\\nsample_15x,15,samples1,C1,30\\nsample_20x,20,samples1,D1,30\\nsample_25x,25,samples1,A2,30\\nsample_30x,30,samples1,B2,30\\nsample_40x,40,samples1,C2,30\\nsample_50x,50,samples1,D2,30"}""")
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

  #### START OF FUNCTION DEFINITIONS ###

  # slow retract mix function
  def m20_slow_retract_transfer_mix(volume, source, destination, mix_volume, mix_times):
    m20.pick_up_tip()
    #aspirate sample
    m20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
    m20.aspirate(volume, source.bottom(bottom_offset))
    m20.flow_rate.aspirate *= 10  # modulate flow rate for removal
    m20.move_to(source.top(),speed = 5) #slower retraction
    #dispense sample
    m20.dispense(volume,destination.bottom(bottom_offset))
    #mix 5 times with 10 ul
    m20.mix(mix_times, mix_volume, destination.bottom(bottom_offset))
    def_pipette = m20.flow_rate.blow_out
    #slow blowout
    m20.flow_rate.blow_out = 0.5
    m20.blow_out()
    m20.flow_rate.blow_out = def_pipette #set to default blow out rate
    m20.move_to(destination.top(),speed = 5) #slower retraction
    m20.drop_tip()
    
  def m20_slow_retract_transfer_mix_before(volume, source, destination, mix_volume_before, mix_times_before):
    m20.pick_up_tip()
    #aspirate sample
    m20.mix(mix_times_before, mix_volume_before, source.bottom(bottom_offset))
    m20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
    m20.aspirate(volume, source.bottom(bottom_offset))
    m20.flow_rate.aspirate *= 10  # modulate flow rate for removal
    m20.move_to(source.top(),speed = 5) #slower retraction
    #dispense sample
    m20.dispense(volume,destination.bottom(bottom_offset))
    #mix 5 times with 10 ul
    def_pipette = m20.flow_rate.blow_out
    #slow blowout
    m20.flow_rate.blow_out = 0.5
    m20.blow_out()
    m20.flow_rate.blow_out = def_pipette #set to default blow out rate
    m20.move_to(destination.top(),speed = 5) #slower retraction
    m20.drop_tip()
    
  # slow retract without mix function
  def m20_slow_retract_transfer_W_O_mix(volume, source, destination):
    volume_tmp_times = math.ceil(volume/20) # 20ul max volume of p20
    m20.pick_up_tip()
    for k in range(0,volume_tmp_times):
      #aspirate sample
      m20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
      m20.aspirate(volume/volume_tmp_times, source.bottom(bottom_offset))
      m20.flow_rate.aspirate *= 10  # modulate flow rate for removal
      m20.move_to(source.top(),speed = 5) #slower retraction
      #dispense sample
      m20.dispense(volume/volume_tmp_times,destination.bottom(bottom_offset))
      def_pipette = m20.flow_rate.blow_out
      #slow blowout
      m20.flow_rate.blow_out = 0.5
      m20.blow_out()
      m20.flow_rate.blow_out = def_pipette #set to default blow out rate
      m20.move_to(destination.top(),speed = 5) #slower retraction
    m20.drop_tip()  

    
  # transfer slow retract function
  def p20_slow_retract_transfer(volume, source, destination):
    volume_tmp_times = math.ceil(volume/20) # 20ul max volume of p20
    for k in range(0,volume_tmp_times):
      p20.pick_up_tip()
      #aspirate sample
      p20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
      p20.aspirate(volume/volume_tmp_times, source.bottom(bottom_offset))
      p20.flow_rate.aspirate *= 10  # modulate flow rate for removal
      p20.move_to(source.top(),speed = 5) #slower retraction
      #dispense sample
      p20.dispense(volume/volume_tmp_times,destination.bottom(bottom_offset))
      p20.drop_tip()
    

 # transfer slow retract mix function for standard
  def p20_slow_retract_transfer_mixing(volume, source, source_tmp, destination, destination_tmp, mix_volume, mix_times):
    volume_tmp_times = math.ceil(volume/20) # 20ul max volume of p20
    for k in range(0,volume_tmp_times):
      p20.pick_up_tip()
      #aspirate sample
      p20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s
      p20.aspirate(volume/volume_tmp_times, source)
      p20.flow_rate.aspirate *= 10  # modulate flow rate for removal
      p20.move_to(source_tmp.top(),speed = 5) #slower retraction
      #dispense sample
      p20.dispense(volume/volume_tmp_times,destination.bottom(bottom_offset))
      p20.mix(mix_times, mix_volume, destination.bottom(bottom_offset))
      #slow blowout
      p20_def_pipette = p20.flow_rate.blow_out
      p20.flow_rate.blow_out = 0.5
      p20.blow_out()
      p20.flow_rate.blow_out = p20_def_pipette
      p20.move_to(destination_tmp.top(),speed = 5) #slower retraction
      p20.drop_tip()

  #### END OF FUNCTION DEFINITIONS ###
  
  
  #std. dilution: transfer 2xBSA diluent
  m20_slow_retract_transfer_W_O_mix(volume = standard_dilution_end_volume,
                                    source = reagent_plate.wells_by_name()["A1"],
                                    destination = prep_plate.wells_by_name()["A1"])


  #refine 2xBSA diluent with P20 / use one tip
  for i in range(0,7): 
    p20_slow_retract_transfer(volume = std_volumes[i],
      source = prep_plate.wells_by_name()[std_positions[i]],
      destination = protocol.fixed_trash["A1"])
    
         
  #BSA standard dilution
  for i in range(0,7): 
    p20_slow_retract_transfer_mixing(volume = std_volumes[i],
      source = samples2.wells_by_name()["D6"].bottom(vol_height_1_5_ml(initial_volume = standard_initial_volume, aspirate_volume=std_volumes[i], height_below_mm=5)),
      source_tmp = samples2.wells_by_name()["D6"],
      destination = prep_plate.wells_by_name()[std_positions[i]],
      destination_tmp = prep_plate.wells_by_name()[std_positions[i]],
      mix_volume = 10,
      mix_times = 2)
    #standard height
    standard_initial_volume = standard_initial_volume - std_volumes[i]

  
  #dispense diluent for sample dilution
  for i in range(0,math.ceil(len(SampleTransfer)/8)):
    m20_slow_retract_transfer_W_O_mix(volume = sample_dilution_end_volume,
                                      source = reagent_plate.wells_by_name()["A2"],
                                      destination = prep_plate.wells_by_name()[prep_columns_positions[i]])

  #refine sample diluent with P20 / use one tip
  for i in range(0,len(SampleTransfer)): 
    p20_slow_retract_transfer(volume = (sample_dilution_end_volume-SampleTransfer[i][7])+SampleTransfer[i][6],
      source = prep_plate.wells_by_name()[SampleTransfer[i][5]],
      destination = protocol.fixed_trash["A1"])

   
  #sample dilution
  for i in range(0,len(SampleTransfer)):
    if SampleTransfer[i][2] == "samples1": 
      p20_slow_retract_transfer_mixing(volume = SampleTransfer[i][6],
        source = samples1.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=3)),
        source_tmp = samples1.wells_by_name()[SampleTransfer[i][3]],
        destination = prep_plate.wells_by_name()[SampleTransfer[i][5]],
        destination_tmp = prep_plate.wells_by_name()[SampleTransfer[i][5]],
        mix_volume = 10,
        mix_times = 2)
       
    else: p20_slow_retract_transfer_mixing(volume = SampleTransfer[i][6],
            source = samples2.wells_by_name()[SampleTransfer[i][3]].bottom(vol_height_1_5_ml(initial_volume=float(SampleTransfer[i][4]),aspirate_volume=SampleTransfer[i][6],height_below_mm=3)),
            source_tmp = samples2.wells_by_name()[SampleTransfer[i][3]],
            destination = prep_plate.wells_by_name()[SampleTransfer[i][5]],
            destination_tmp = prep_plate.wells_by_name()[SampleTransfer[i][5]],
            mix_volume = 10,
            mix_times = 2)
    
    #sample height adjustment
    for k in range(0,len(SampleTransfer)):
      if SampleTransfer[i][0]==SampleTransfer[k][0]: SampleTransfer[k][4] = float(SampleTransfer[k][4])-SampleTransfer[i][6]
    

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
  
  #shake plate
  hs_mod.set_and_wait_for_shake_speed(2000)
  protocol.delay(minutes=2)
  hs_mod.deactivate_shaker()
                  
  # distribute 2x buffer to BCA plate
  m20_slow_retract_transfer_mix(volume = 5,
    source = reagent_plate.wells_by_name()["A3"],
    destination = BCA_plate.wells_by_name()["A1"],
    mix_volume = 5,
    mix_times = 2)
  
  m20_slow_retract_transfer_mix(volume = 5,
    source = reagent_plate.wells_by_name()["A3"],
    destination = BCA_plate.wells_by_name()["A2"],
    mix_volume = 5,
    mix_times = 2)
  
  # distribute 2x std. to BCA plate
  m20_slow_retract_transfer_mix(volume = 5,
    source = prep_plate.wells_by_name()["A1"],
    destination = BCA_plate.wells_by_name()["A1"],
    mix_volume = 5,
    mix_times = 2)
  
  
  m20_slow_retract_transfer_mix(volume = 5,
    source = prep_plate.wells_by_name()["A1"],
    destination = BCA_plate.wells_by_name()["A2"],
    mix_volume = 5,
    mix_times = 2)
  
  #distribute sample to BCA plate
  for i in range(0, math.ceil(sample_number/8)): 
    m20_slow_retract_transfer_mix(volume = 10,
      source = prep_plate.wells_by_name()[prep_columns_positions[i]],
      destination = BCA_plate.wells_by_name()[BCA_columns_position[i][0]],
      mix_volume = 10,
      mix_times = 2)
    m20_slow_retract_transfer_mix(volume = 10,
      source = prep_plate.wells_by_name()[prep_columns_positions[i]],
      destination = BCA_plate.wells_by_name()[BCA_columns_position[i][1]],
      mix_volume = 10,
      mix_times = 2)
 
  # open latch HS
  hs_mod.open_labware_latch()

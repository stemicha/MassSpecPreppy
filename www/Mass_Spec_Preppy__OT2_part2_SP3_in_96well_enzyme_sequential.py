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
    _all_values = json.loads("""{"csv_sample":"sample,protein concentration (µg/µl),OT-2 slot,OT-2 position,volume,preparation plate position\\nHEK__0.5ug_ul__1,0.5,samples1,A1,20,A1\\nHEK__0.5ug_ul__2,0.5,samples1,B1,15,B1\\nHEK__0.5ug_ul__3,0.5,samples1,C1,10,C1\\nHEK__1.0ug_ul__1,1,samples1,D1,20,D1\\nHEK__1.0ug_ul__2,1,samples1,A2,15,E1\\nHEK__1.0ug_ul__3,1,samples1,B2,10,F1\\nHEK__1.5ug_ul__1,1.5,samples1,C2,20,G1\\nHEK__1.5ug_ul__2,1.5,samples1,D2,15,H1\\nHEK__1.5ug_ul__3,1.5,samples1,A3,10,A2\\nHEK__2.0ug_ul__1,2,samples1,B3,20,B2\\nHEK__2.0ug_ul__2,2,samples1,C3,15,C2\\nHEK__2.0ug_ul__3,2,samples1,D3,10,D2","sample_amount":"5","trypsin_ratio":"25","trypsin_stock_concentration":"100"}""")
    return [_all_values[n] for n in names]



#specify meta data
metadata = {
     "protocolName": "Mass Spec Preppy: step 2 > SP3 preparation with sequential enzyme digest",
     "apiLevel": "2.12",
     "description": "This part of the Mass Spec Preppy workflow will perform the SP3 protocol up to the disgest (incubation)",
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

  
  #calculate trypsin volumn and digest buffer volume // trypsin stock is 20ng/µl
  volume_trpysin = (sample_amount*1000/trypsin_ratio)/trypsin_stock_concentration
  volume_LysC = (sample_amount*1000/LysC_ratio)/LysC_stock_concentration
  digest_buffer_volume = 22-volume_trpysin-volume_LysC # 22 µl final volume of digest
  
  # csv_sample --> nested list
  SampleTransfer = [[val.strip() for val in line.split(",")]
             for line in csv_sample.splitlines()
             if line.split(",")[0].strip()][1:]
  # sample numbers
  sample_number = len(SampleTransfer)
  num_cols = math.ceil(sample_number/8)

  #paramters for magnet
  OFFSET_RADIAL = 1.5  # in mm
  OFFSET_Z = 3.0  # in mm
  OFFSET_Z_remove = 0.5 # in mm
  MAGNET_HEIGHT = 12.0  # in mm
  INCUBATION_TIME = 3.0  # in minutes
  
  #load magnetic module
  magnet_module = protocol.load_module("magnetic module gen2", "1")
  #preparation plate on magnet
  magplate = magnet_module.load_labware("nest_96_wellplate_100ul_pcr_full_skirt")
  mag_samples = magplate.rows()[0][:num_cols]

  #prep plate column positions
  prep_plate_columns_position = ["A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12"]

  #setup labware
  #tips
  tiprack300_1 = protocol.load_labware("opentrons_96_tiprack_300ul", "10",label = "OT_96_tiprack_300ul_1")
  tiprack300_2 = protocol.load_labware("opentrons_96_tiprack_300ul", "7",label = "OT_96_tiprack_300ul_2")
  tiprack300_3 = protocol.load_labware("opentrons_96_tiprack_300ul", "4",label = "OT_96_tiprack_300ul_3")
  tiprack300_4 = protocol.load_labware("opentrons_96_tiprack_300ul", "11",label = "OT_96_tiprack_300ul_4")
  tiprack300_5 = protocol.load_labware("opentrons_96_tiprack_300ul", "8",label = "OT_96_tiprack_300ul_5")
  tiprack300_6 = protocol.load_labware("opentrons_96_tiprack_300ul", "5",label = "OT_96_tiprack_300ul_6")

  tiprack20_1 = protocol.load_labware("opentrons_96_tiprack_20ul", "9",label = "OT_96_tiprack_20ul_1")
  tiprack20_2 = protocol.load_labware("opentrons_96_tiprack_20ul", "6",label = "OT_96_tiprack_20ul_2")
  
  #pipettes
  m20 = protocol.load_instrument("p20_multi_gen2", mount = "right", tip_racks = [tiprack20_1,tiprack20_2])
  m300 = protocol.load_instrument("p300_multi_gen2", mount = "left", tip_racks = [tiprack300_1,tiprack300_2,tiprack300_3,tiprack300_4,tiprack300_5,tiprack300_6])

  #reagent plate
  #column 1 = 8ml 95%EtOH 5.28ml for first step used
  #column 2 = 8ml 95%EtOH 5.28ml for first step used
  #column 3 = 8ml 95%EtOH 7.2ml for second step used
  #column 4 = 8ml 95%EtOH 7.2ml for second step used
  #column 5 = 8ml 80%Ethanol 7.2ml for first step used
  #column 6 = 8ml 80%Ethanol 7.2ml for first step used
  #column 7 = 8ml 80%Ethanol 7.2ml for second step used
  #column 8 = 8ml 80%Ethanol 7.2ml for second step used
  #column 9 = 4ml digest buffer 1.92ml for digest buffer addition
  reagents = protocol.load_labware("nest_12_reservoir_15ml", "2",label = "reagents")
  
  #trypsin plate / column 1 26µl per well of stock when 2µl of are used
  enzyme_plate = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "3",label = "enzyme_plate")


  #volume from previous step = 20.625µl
  

  ##############################################
  # remove supernatant function
  ##############################################
  def remove_supernatant(vol, waste_position, park=False):
        m300.flow_rate.aspirate /= 8  # modulate flow rate for removal
        for i, m in enumerate(mag_samples):
            side = -1 if i % 2 == 0 else 1  # select side away from beads
            loc = m.bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z_remove))
            m300.pick_up_tip()
            m300.move_to(m.center())
            m300.transfer(vol, loc, waste_position, new_tip='never', air_gap=20)
            m300.blow_out(waste_position)
            m300.drop_tip()
        m300.flow_rate.aspirate *= 8
        
  ##############################################
  # resuspend beads m300 function
  ##############################################
  def resuspend_beads_m300(vol, buffer_position):
        def resuspend_mix(side, vol, reps, well):
            # offset + 2 mm in z-axis and 1.4fold to x axis for dispensing at reaction tube wall above bead pellet
            bead_loc = well.bottom().move(Point(x=side*OFFSET_RADIAL*1.4, z=OFFSET_Z+2))
            #bead_loc = well.bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z))
            for _ in range(reps):
                m300.aspirate(vol*0.7, well.bottom(OFFSET_Z)) #offset from bottom
                m300.dispense(vol*0.7, bead_loc)
      
        #disengage magnet
        magnet_module.disengage()
    
        for i, m in enumerate(mag_samples):
            side = 1 if i % 2 == 0 else -1
            m300.pick_up_tip()
            if i<6:
                m300.transfer(vol, reagents.wells_by_name()[buffer_position[0]], m.top(), air_gap=20,new_tip = "never", mix_before = (1,vol)) # mix before >> pre-wet tip
            else:
                m300.transfer(vol, reagents.wells_by_name()[buffer_position[1]], m.top(), air_gap=20,new_tip = "never", mix_before = (1,vol)) # mix before >> pre-wet tip
            resuspend_mix(side,vol,reps = 5, well = m)  # resuspend
            m300.drop_tip()
        
        #engage magnet
        m300.move_to(locPause)
        magnet_module.engage(height=MAGNET_HEIGHT)
        protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')

  ##############################################
  # initial resuspend beads m300 function
  ##############################################
  def initial_resuspend_beads_m300(vol, buffer_position):
        def resuspend_mix(side, vol, reps, well):
            bead_loc = well.bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z))
            for _ in range(reps):
                m300.aspirate(vol*0.7, well.bottom(OFFSET_Z)) #1 mm from bottom
                m300.dispense(vol*0.7, bead_loc)
      
        #disengage magnet
        magnet_module.disengage()
    
        for i, m in enumerate(mag_samples):
            side = 1 if i % 2 == 0 else -1
            m300.pick_up_tip()
            if i<6:
                m300.transfer(vol, reagents.wells_by_name()[buffer_position[0]], m.top(), air_gap=20,new_tip = "never", mix_before = (1,vol)) # mix before >> pre-wet tip
            else:
                m300.transfer(vol, reagents.wells_by_name()[buffer_position[1]], m.top(), air_gap=20,new_tip = "never", mix_before = (1,vol)) # mix before >> pre-wet tip
            resuspend_mix(side,vol,reps = 15, well = m)  # resuspend
            m300.drop_tip()
        
       
  ##############################################
  # resuspend beads m20 function
  ##############################################
  def resuspend_beads_m20(vol, buffer):
        def resuspend_mix(side, vol, reps, well):
            bead_loc = well.bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z))
            for _ in range(reps):
                m20.aspirate(vol, well.bottom(0.5))
                m20.dispense(vol, bead_loc)

        magnet_module.disengage()

        for i, m in enumerate(mag_samples):
            side = 1 if i % 2 == 0 else -1
            m20.pick_up_tip()
            m20.transfer(vol, buffer, m.bottom(1),
                          new_tip='never')
            resuspend_mix(side,vol,reps = 15, well = m)  # resuspend
            m20.drop_tip()
        
  
  
  
  ##################################################
  # add 95% ACN to a final concentration of 80% ACN
  ##################################################
  initial_resuspend_beads_m300(vol = 110, buffer_position = ["A1","A2"]) # 95% ACN; 15 reps
  # vol = 130.625
  
  #delay step
  protocol.delay(minutes=5,msg="incubation for 5 minutes.")
  
  #remove supernant / 1. ACN step 80%
  magnet_module.engage(height=MAGNET_HEIGHT)
  protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')
  remove_supernatant(vol = 140, waste_position = reagents.wells_by_name()["A12"])# collect ACN waste in A12
  
  #perform washing steps / 1. EtOH wash step 80%
  resuspend_beads_m300(vol = 150, buffer_position = ["A5","A6"]) # 80% EtOH
  
  #remove supernant / 1. EtOH wash step 80%
  remove_supernatant(vol = 155, waste_position = protocol.fixed_trash["A1"])
  
  #perform washing steps / 2. EtOH wash step 80%
  resuspend_beads_m300(vol = 150, buffer_position = ["A7","A8"]) # 80% EtOH
  
  #remove supernant / 2. EtOH wash step 80%
  remove_supernatant(vol = 155, waste_position =protocol.fixed_trash["A1"])

  #tips refill if more samples than N=72
  if sample_number>72:
    protocol.pause("replace all empty and used 300ul tip boxes with new full 300 ul tip boxes, empty waste and resume.")
    m300.reset_tipracks()
    
  #perform washing steps / 2. ACN wash step 95%
  resuspend_beads_m300(vol = 150, buffer_position = ["A3","A4"]) # 95%ACN
  
  #remove supernant / 2. ACN wash step 95%
  remove_supernatant(vol = 170, waste_position = reagents.wells_by_name()["A11"])
  
  #air dry beads
  m300.move_to(locPause)
  protocol.delay(minutes=15, msg="air dry beads for 15 minutes")
  
  #transfer digest buffer
  for i in range(0, math.ceil(sample_number/8)):
    m20.transfer(digest_buffer_volume, 
                 reagents.wells_by_name()["A9"], 
                 magplate.wells_by_name()[prep_plate_columns_position[i]].bottom(1), #1mm from bottom
                 new_tip = "always",
                 touch_tip = False)
  
  #transfer LysC stock to solution
  for i in range(0, math.ceil(sample_number/8)):
    m20.transfer(volume_LysC, 
                 enzyme_plate.wells_by_name()["A1"].bottom(0.5), # 0.5mm from bottom
                 magplate.wells_by_name()[prep_plate_columns_position[i]].bottom(1), #1mm from bottom
                 new_tip = "always",
                 touch_tip = False)
                 
  #disengage magnet module         
  magnet_module.disengage()
  
  #pause step
  protocol.pause("take preparation plate out, seal it > place plate in MixMate and shake for at least 5 minutes at 3000rpm > short centrifugation with table plate centrifuge > put plate for 3 minutes to a sonication bath and incubate it at 37°C for 3 hours. Replace 20ul tips with a new FULL tip racks, add trypsin in the reagent plate and resume!")
  
  #reset 20ul tip rack
  m20.reset_tipracks()
  
  magnet_module.engage(height=MAGNET_HEIGHT)
  protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')
  #transfer trypsin stock to solution
  for i in range(0, math.ceil(sample_number/8)):
    m20.transfer(volume_trpysin, 
                 enzyme_plate.wells_by_name()["A2"].bottom(0.5), # 0.5mm from bottom
                 magplate.wells_by_name()[prep_plate_columns_position[i]].bottom(1), #1mm from bottom
                 new_tip = "always",
                 touch_tip = False)
                 
  #disengage magnet module         
  magnet_module.disengage()

 #comment step
  protocol.comment("take preparation plate out, seal it > place plate in MixMate and shake for at least 5 minutes at 3000rpm > short centrifugation with table plate centrifuge > put plate for 3 minutes to a sonication bath and incubate it at 37°C for 15-16hours.")

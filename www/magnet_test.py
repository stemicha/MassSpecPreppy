#import depedencies
## install in R
## library(reticulate)
## py_install(packages = "opentrons",pip = T)
# dependencies
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
     "protocolName": "Mass Spec Preppy: step 2 > SP3 preparation",
     "apiLevel": "2.12",
     "description": "This part of the Mass Spec Preppy workflow will perform the SP3 protocol up to the disgest (incubation)",
     "author": "Stephan Michalik <stephan.michalik@uni-greifswald.de>"}


#run protocol from here on
#remember python counts from 0 in iteration and postion in lists etc.!!!!
def run(protocol: protocol_api.ProtocolContext):
  
  #get csv values from JSON above
  [csv_sample, sample_amount, trypsin_ratio, trypsin_stock_concentration] = get_values("csv_sample","sample_amount","trypsin_ratio", "trypsin_stock_concentration")

  #convert to float number
  sample_amount = float(sample_amount)
  trypsin_ratio = float(trypsin_ratio)
  trypsin_stock_concentration = float(trypsin_stock_concentration)

  
  #calculate trypsin volumn and digest buffer volume // trypsin stock is 20ng/µl
  volume_trpysin = (sample_amount*1000/trypsin_ratio)/trypsin_stock_concentration
  digest_buffer_volume = 22-volume_trpysin
  
  # csv_sample --> nested list
  SampleTransfer = [[val.strip() for val in line.split(",")]
             for line in csv_sample.splitlines()
             if line.split(",")[0].strip()][1:]
  # sample numbers
  sample_number = len(SampleTransfer)
  num_cols = math.ceil(sample_number/8)

  #paramters for magnet
  OFFSET_RADIAL = 2.0  # in mm
  OFFSET_Z = 1.0  # in mm
  MAGNET_HEIGHT = 11.0  # in mm
  INCUBATION_TIME = 0  # in minutes
  
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
  #column 1 = 8ml 95%ACN 5.28ml for first step used
  #column 2 = 8ml 95%ACN 5.28ml for first step used
  #column 3 = 8ml 95%ACN 7.2ml for second step used
  #column 4 = 8ml 95%ACN 7.2ml for second step used
  #column 5 = 8ml 80%Ethanol 7.2ml for first step used
  #column 6 = 8ml 80%Ethanol 7.2ml for first step used
  #column 7 = 8ml 80%Ethanol 7.2ml for second step used
  #column 8 = 8ml 80%Ethanol 7.2ml for second step used
  #column 9 = 4ml digest buffer 1.92ml for digest buffer addition
  reagents = protocol.load_labware("nest_12_reservoir_15ml", "2",label = "reagents")
  
  #trypsin plate / column 1 26µl per well of stock when 2µl of are used
  trypsin_plate = protocol.load_labware("nest_96_wellplate_100ul_pcr_full_skirt", "3",label = "trypsin_plate")

  ##############################################
  # remove supernatant function
  ##############################################
  def remove_supernatant(vol, waste_position, park=False):
        m300.flow_rate.aspirate /= 5  # modulate flow rate for removal
        for i, m in enumerate(mag_samples):
            side = -1 if i % 2 == 0 else 1  # select side away from beads
            loc = m.bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z))
            m300.pick_up_tip()
            m300.move_to(m.center())
            m300.transfer(vol, loc, waste_position, new_tip='never', air_gap=20)
            m300.blow_out(waste_position)
            m300.drop_tip()
        m300.flow_rate.aspirate *= 5
        
  ##############################################
  # resuspend beads m300 function
  ##############################################
  def resuspend_beads_m300(vol, buffer_position):
    def resuspend_mix(side, vol, reps, well):
      bead_loc = well.bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z))
      for _ in range(reps):
        m300.aspirate(vol, well.bottom(0.5))
        m300.dispense(vol, bead_loc)
      
      #disengage magnet
      magnet_module.disengage()
    
      for i, m in enumerate(mag_samples):
        side = 1 if i % 2 == 0 else -1
        m300.pick_up_tip()
        if i<6:
          m300.transfer(vol, reagents.wells_by_name()[buffer_position[0]], m.top(), air_gap=20,
                          new_tip = "never", mix_before = (1,vol)) # mix before >> pre-wet tip
        else:
          m300.transfer(vol, reagents.wells_by_name()[buffer_position[1]], m.top(), air_gap=20,
                          new_tip = "never", mix_before = (1,vol)) # mix before >> pre-wet tip
        resuspend_mix(side,vol,reps = 5, well = m)  # resuspend
        m300.air_gap(20)
        m300.drop_tip()
        
      #engage magnet
      magnet_module.engage(height=MAGNET_HEIGHT)
      protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')


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
            resuspend_mix(side,vol,reps = 10, well = m)  # resuspend
            m20.drop_tip()
        
  
  #remove supernant / 1. ACN step 80%
  magnet_module.engage(height=MAGNET_HEIGHT)
  protocol.delay(minutes=INCUBATION_TIME, msg=f'Incubating on MagDeck for \
f{INCUBATION_TIME} minutes.')
  remove_supernatant(vol = 130, waste_position = reagents.wells_by_name()["A11"])
  
  #perform washing steps / 1. EtOH wash step 80%
  resuspend_beads_m300(vol = 150, buffer_position = ["A5","A6"]) # 80% EtOH

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
    _all_values = json.loads("""{"csv_sample":"sample,protein concentration (µg/µl),OT-2 slot,OT-2 position,volume,preparation plate position\\nDTG__88__w__9,1.09333918,samples1,A1,69,A1\\nDTG__4__m__9,1.59175242,samples1,B1,69,B1\\nWT__38__w__9,0.99863244,samples1,C1,69,C1\\nHEK_cell_line__01,2.01,samples1,D1,30,D1\\nDTG__21__w__5,1.1093614,samples1,A2,69,E1\\nWT__55__w__9,0.95098809,samples1,B2,69,F1\\nWT__23__w__5,0.84149524,samples1,C2,69,G1\\nWT__29__m__5,1.16932174,samples1,D2,69,H1\\nHEK_cell_line__02,2.01,samples1,A3,30,A2\\nWT__42__m__9,1.16824911,samples1,B3,69,B2\\nWT__59__w__9,1.16288698,samples1,C3,69,C2\\nWT__40__m__9,1.12968012,samples1,D3,69,D2\\nDTG__89__w__9,1.16610406,samples1,A4,69,E2\\nWT__13__w__5,1.12968012,samples1,B4,69,F2\\nDTG__63__m__5,1.34055068,samples1,C4,69,G2\\nWT__28__m__5,1.07733397,samples1,D4,69,H2\\nDTG__22__w__5,0.94676097,samples1,A5,69,A3\\nWT__58__w__9,1.19830709,samples1,B5,69,B3\\nDTG__26__w__5,0.66319424,samples1,C5,69,C3\\nDTG__78__w__9,0.91826162,samples1,D5,69,D3\\nDTG__24__m__5,0.82367657,samples1,A6,69,E3\\nHEK_cell_line__03,2.01,samples1,B6,30,F3\\nWT__39__m__9,1.08053363,samples1,C6,69,G3\\nWT__49__m__5,1.16824911,samples1,D6,69,H3\\nHEK_cell_line__04,2.01,samples2,A1,30,A4\\nDTG__8__w__9,1.06667343,samples2,B1,69,B4\\nDTG__71__m__9,0.92037068,samples2,C1,69,C4\\nDTG__65__m__5,1.08586794,samples2,D1,69,D4\\nWT__48__m__5,1.21442954,samples2,A2,69,E4\\nWT__14__w__5,1.28550995,samples2,B2,69,F4\\nDTG__12__w__5,1.03367542,samples2,C2,69,G4\\nDTG__90__m__9,1.18541898,samples2,D2,69,H4\\nWT__7__m__9,1.24886634,samples2,A3,194,A5\\nDTG__64__m__5,0.89718849,samples2,B3,194,B5\\nDTG__77__m__9,0.94464789,samples2,C3,194,C5\\nWT__15__w__5,1.26394875,samples2,D3,194,D5\\n42-1_patient,1.35128526,samples2,A4,194,E5\\n18-1_control,1.61013074,samples2,B4,194,F5\\n22-1_17-2_patient_t2,2.27569644,samples2,C4,194,G5\\nHEK_cell_line__05,2.01,samples2,D4,30,H5\\n20-1_control,1.60896266,samples2,A5,194,A6\\n39-1_control,1.45349238,samples2,B5,194,B6\\nHEK_cell_line__06,2.01,samples2,C5,30,C6\\n16-2_patient_t2,1.46946243,samples2,D5,194,D6\\n27-1_control,0.99018173,samples2,A6,194,E6\\n08-1_patient,1.51409809,samples2,B6,194,F6\\n33-1_control,1.47745707,samples2,C6,194,G6\\n16-1_patient,1.20214544,samples2,D6,194,H6\\n26-1_control,1.06795816,samples3,A1,194,A7\\n32-1_control,1.33996432,samples3,B1,194,B7\\nHEK_cell_line__07,2.01,samples3,C1,30,C7\\n11-1_patient,1.8984765,samples3,D1,194,D7\\n21-1_control,1.40344088,samples3,A2,194,E7\\n38-1_patient,1.44665535,samples3,B2,194,F7\\n05-1_control,1.26287114,samples3,C2,194,G7\\n19-1_12-2_patient_t2,2.6019484,samples3,D2,194,H7\\n13-1_patient,1.34562413,samples3,A3,194,A8\\n31-1_control,1.45577233,samples3,B3,194,B8\\n12-1_patient,1.69631231,samples3,C3,194,C8\\n02-1_control,1.48831359,samples3,D3,194,D8\\nHEK_cell_line__08,2.01,samples3,A4,30,E8\\n17-1_patient,1.66659273,samples3,B4,194,F8\\n41-1_control,1.31168305,samples3,C4,194,G8\\n24-1_patient,1.20891492,samples3,D4,194,H8\\n09-1_patient,1.26083651,samples3,A5,194,A9\\n03-1_control,1.04111996,samples3,B5,194,B9\\n14-1_patient,1.0735946,samples3,C5,194,C9\\nHEK_cell_line__09,2.01,samples3,D5,30,D9\\n34-1_patient,0.96538597,samples3,A6,194,E9\\n30-1_control,1.32525456,samples3,B6,194,F9\\n10-1_patient,1.85753688,samples3,C6,194,G9\\n28-1_control,1.44096094,samples3,D6,194,H9\\n35-1_24-2_patient_t2,1.32864841,samples4,A1,194,A10\\n01-1_control,1.87148627,samples4,B1,194,B10\\nHEK_cell_line__10,2.01,samples4,C1,30,C10\\n25-1_patient,1.65004237,samples4,D1,194,D10\\n06-1_control,1.44741431,samples4,A2,194,E10\\n40-1_patient,1.43413122,samples4,B2,194,F10\\n23-1_control,3.91531519,samples4,C2,194,G10\\n08-2_patient_t2,1.99706367,samples4,D2,194,H10\\n04-1_control,1.18219837,samples4,A3,194,A11\\nHEK_cell_line__11,2.01,samples4,B3,30,B11\\n37-1_patient,1.11756214,samples4,C3,194,C11\\n14-2_patient_t2,1.6820216,samples4,D3,194,D11\\n36-1_patient,1.2789079,samples4,A4,194,E11\\n07-1_control,1.46465001,samples4,B4,194,F11\\n34-4d_patient,0.87561893,samples4,C4,194,G11\\n36-6d_patient,0.98171071,samples4,D4,194,H11\\npatient_pool_1,1.25294616,samples4,A5,194,A12\\npatient_pool_2,1.4850292,samples4,B5,194,B12\\ncontrol_pool_1,1.49690213,samples4,C5,194,C12\\ncontrol_pool_2,1.28003761,samples4,D5,194,D12\\ncontrol_pool_3,1.89287019,samples4,A6,194,E12\\ncontrol_pool_4,1.70944804,samples4,B6,194,F12\\nHEK_cell_line__12,2.01,samples4,C6,30,G12\\nHEK_cell_line__13,2.01,samples4,D6,30,H12","sample_amount":"4","trypsin_ratio":"50","trypsin_stock_concentration":"40","LysC_ratio":"100","LysC_stock_concentration":"20","EvoTips_amount_ng":"400"}""")
    return [_all_values[n] for n in names]



#specify meta data
metadata = {
     "protocolName": "Mass Spec Preppy: step 3 > peptide purification and vial elution of digest",
     "apiLevel": "2.12",
     "description": "This part of the Mass Spec Preppy workflow will perform the elution into MS vials",
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
  OFFSET_Z_remove = 1.0 # in mm
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
  tiprack20_1 = protocol.load_labware("opentrons_96_tiprack_20ul", "4",label = "OT_96_tiprack_20ul_1")
  tiprack20_2 = protocol.load_labware("opentrons_96_tiprack_20ul", "5",label = "OT_96_tiprack_20ul_2")
 
  #pipettes
  p20 = protocol.load_instrument("p20_single_gen2", mount = "left", tip_racks = [tiprack20_2])
  m20 = protocol.load_instrument("p20_multi_gen2", mount = "right", tip_racks = [tiprack20_1])

  #reagent plate
  #column 1 = 4ml 5%TFA for facidification
  reagents = protocol.load_labware("nest_12_reservoir_15ml", "2",label = "reagents")
  
  #vial labware (48 samples per slot)
  vials1 = protocol.load_labware("cfungeneot2msvialrack_48_tuberack_100ul", "6",label = "MS-vials_1")
  vials2 = protocol.load_labware("cfungeneot2msvialrack_48_tuberack_100ul", "3",label = "MS-vials_2")


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


  # elute samples
  #################
  
  #engage magnet
  magnet_module.engage(height=MAGNET_HEIGHT)
  protocol.delay(minutes=5, msg="Incubating on MagDeck for 5 minutes.")

  p20.flow_rate.aspirate /= 10  # modulate flow rate for removal / On API Version 2.6 and subsequent: 7.56 µL/s

  for i in range(0,sample_number):
    k = math.ceil((i+1)/8) #side offset
    side = 1 if k % 2 == 0 else -1  # select side away from beads
    loc = magplate.wells()[i].bottom().move(Point(x=side*OFFSET_RADIAL, z=OFFSET_Z_remove))
    if(i<48):
      p20.transfer(24, loc, vials1.wells()[i].bottom(3), new_tip="once")
    else:
      p20.transfer(24, loc, vials2.wells()[i-48].bottom(3), new_tip="once")
 
  p20.flow_rate.aspirate *= 10  # modulate flow rate for removal

  #disengage magnet
  magnet_module.disengage()

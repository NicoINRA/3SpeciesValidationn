# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher Puechabon_LFMC avec Van-Genuchten formulation 
# Authors : Nicolas Martin (nicolas.martin@inrae.fr)
#           Julien Ruffault (julien.ruff@gmail.com)
# Date : 17/01/2022           
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

# load SurEau-Ecos ------------------------------------------------------------
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) 
modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis',
                                             defoliation = T,
                                             stomatalRegFormulation = "Turgor",
                                             PedoTransferFormulation="VG") 

#------------------
#O3HP
climateData_path          <- paste0(mainDir,'/Input_parameters/Climat/O3HP/O3HP_SAFRAN_histo.csv')
soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil/Soil_O3HP_VG.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/Vegetation/vegetation_O3HP.csv')
output_path               <-  paste0(mainDir,'/scripts_base_simulations/test_Valid_O3HP.csv')

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2013,                       
                                                      endYearSimulation = 2020,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

stand_parameters      <- create.stand.parameters(LAImax = 2.2, lat = 43.75, lon = 3.6)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)

vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)
vegetation_parameters$vol_Stem <- 10
vegetation_parameters$gsMax <- 250
PlotTheStandAndPlant(vegetation_parameters, soil_parameters, modeling_options, openWindow=T)

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)



#----------
#FontBlanche
climateData_path          <- paste0(mainDir,'/Input_parameters/Climat/FontBlanche/Font Blanche_SAFRAN_histo.csv')
climateData_path          <- paste0(mainDir,'/Input_parameters/Climat/FontBlanche/data_FON_corrected.csv')
soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil/Soil_Puechabon_VG2.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/Vegetation/vegetation_FontBlanche.csv')
output_path               <-  paste0(mainDir,'/scripts_base_simulations/test_Valid_FontBlanche.csv')

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2012,                       
                                                      endYearSimulation = 2018,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

stand_parameters      <- create.stand.parameters(LAImax = 2.2, lat = 43.75, lon = 3.6)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)

vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)

PlotTheStandAndPlant(vegetation_parameters, soil_parameters, modeling_options, openWindow=T)

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)


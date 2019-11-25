#!/bin/bash

#This script is design to run the power simulations on a multi-core computer.
#It is set up to run on a TORQUE system or as a simple BASH script.
#It takes all parameters from a file (here parameter_matrix.csv)
#and based on the number of cores used (-n nodeuse variable), splits it into segments 
#to be analyzed in parallel (specifically N lines in parameter file/ n cores)

#You can control the number of simulations sets run using the -r run parameter
#The default is 10. Each set is hard coded to run 100 simulations on a given
#set of parameters. You can change this by going into the simulation code and 
#changing the number of simulations parameter

#Last, the parameters used in simulation can be changed by altering the parameter
#matrix file. The simulation code itself contains information on how parameters
#can be altered and a custom matrix made


#To run as a TORQUE script, paste the following uncomment code into the shell
# for runno in {1..5}
 # do
   #wd=$(pwd)
 # # qsub simulate_power.sh -lwalltime=3:00:00 -d $wd -e errandout/ -o errandout/ -F "-n 7 -r 10"
 # done

 
#To run as a BASH script, paste the following uncomment code into the shell
 # for runno in {1..5}
 # do
   #wd=$(pwd)
 # bash simulate_power.sh -n 7 -r 10
 # done

while getopts n:r: option
do
  case "${option}"
    in
      n) nodeuse=${OPTARG};;
      r) run=${OPTARG};;
    esac
done

#want to split N commands into K jobs, run L jobs at a time

ncommands=$(wc -l parameter_matrix.csv   | awk '{print $1}')

nodesize=$nodeuse
totjobs=$(( ($ncommands + $nodeuse - 1 ) / $nodeuse ))


for i in $(seq 1 $nodesize $totjobs)
 do
  #Run jobs K..K+node num
  jstart=$i
  jstop=$(($i + $nodesize - 1))
  min=$([ $ncommands -le $jstop ] && echo "$ncommands" || echo "$jstop")
  jstop=$min
  
  for j in $(seq  $jstart 1 $jstop) 
  do
  linestart=$((($j-1)*$nodeuse +1))
  linestop=$(($j*$nodeuse))

  Rscript /mnt/sdb/genetics/elizabeth_power_simulation/lanc_simulation_v5_loopsims2.txt parameter_matrix.csv $linestart $linestop $run  &
  
  echo $linestart $linestop
  
  done
  wait
  echo "batch $i"
done

# MDX_Tool
A tool for manipulating Yamaha DX7 VMEM and VCED SysEx files  

```
Usage: MDX_Tool.exe -parameters
  Parameters (short and long form):
       -h               --help               This help message
       -i               --info               Information
       -n               --vname              Rename file to the name of the contained voice
       -m               --hname              Rename file to the SHA2-256 hash of the file
       -z               --normalize          Set parameters to be between minimum and maximum allowed values
       -y               --markcorr           Mark files that have parameters outside the min/max limits
                                               Adds .corr as the file extension
       -w               --marknull           Mark files that have nulls ($00) in voice names
                                               Adds .nl as the file extension
       -r               --repair             Repair/extract DX7 VMEM data from files
                                               Adds _DX7_repaired in the file name
                                               Adds _x_DX7_repaired in the file name,
                                               where the x is the number of the dump
                                               in a multi-dump file
       -c               --crop               Crop headers from the VMEM/VCED files
       -s               --split              Split bank (VMEM) into single voices (VCED)
       -x               --xsplit             Split bank (VMEM) into single voices (VCED)
                                               and take the SHA2-256 hash as a file name.
                                               Voice name (10xASCII) is not a part of the hash
       -j               --join               Join single voices (VCED) into a bank (VMEM)
                                               If the file voices.lst exists inside the input directory
                                               - the voices inside the bank will be sorted according to the list

       -f {filename}    --file={filename}    Input file (or output file for -j parameter)
       -d {directory}   --dir={directory}    Output directory for -s and -x parameters
                                               Input directory for -j parameter
                                               If it does not contain a drive letter, a sub-directory in
                                               the current directory will be created.

  Example usage:
       MDX_Tool -i -f my_dx_file.syx
       MDX_Tool -r -f my_dx_file.syx
       MDX_Tool -s -f my_dx_file.syx -d new_directory
       MDX_Tool -j -f my_new_bank.syx -d directory_with_VCEDs
       MDX_Tool --info --file=my_dx_file.syx


Split and Join parameters expect non-corrupted files as input (headerless files are accepted).
  ```
# MDX_Collect
A tool for hashing bank collections  

```
Usage: MDX_Collect.exe -parameters
  Parameters (short and long form):
   -h                 --help                    This help message

   -a                 --analyze                 Make a hash list of VMEM/VCED files in a directory
       -d {directory}     --dir={directory}     Input directory for -a parameter
       -r {directory}     --report={directory}  Output directory for the reports

   -c                 --compare                 Compare two hash lists
       -m {filename}      --master={filename}   Hash list of your collection
       -i {filename}      --incoming={filename} Hash list of incoming collection
       -r {directory}     --report={directory}  Output directory for the reports

  Example usage:
       MDX_Tool -a -d MyCollection -r MyReports
       MDX_Tool -a -d NewFiles -r NewReports
       MDX_Tool -c -m MyCollection.hsl -i NewFiles.hsl -r MyReports
   ```
         
External library HashLib is used in both tools to calculate hash sums: https://github.com/Xor-el/HashLib4Pascal

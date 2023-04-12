# MDX_Tool
A tool for manipulating Yamaha DX7 VMEM and VCED SysEx files  

```
Usage: MDX_Tool.exe -parameters  
  Parameters (short and long form):  
       -h                 --help                   This help message  
       -i                 --info                   Information  
       -r                 --repair                 Repair/extract DX7 VMEM data from files  
       -c                 --crop                   Crop headers from the VMEM/VCED files  
       -s                 --split                  Split bank (VMEM) into single voices (VCED)  
       -x                 --xsplit                 Split bank (VMEM) into single voices (VCED)  
                                                   and take the SHA256 hash as a file name.  
                                                   Voice name (10xASCII) is not a part of the hash  
       -j                 --join                   Join single voices (VCED) into a bank (VMEM)  
                                                   If the file voices.lst exists inside the input directory  
                                                   - the voices inside the bank will be sorted according to the list  
  2nd/3rd parameters (short and long form):  
       -f {filename}      --file={filename}        Input file (or output file for -j parameter)  
       -d {directory}     --dir={directory}        Output directory for -s and -x parameters  
                                                   Input directory for -j parameter  
                                                   If it does not contain a drive letter, a sub-directory in  
                                                   current directory will be created.  
  
  Example usage:  
       MDX_Tool -i -f my_dx_file.syx  
       MDX_Tool -r -f my_dx_file.syx  
       MDX_Tool -s -f my_dx_file.syx -d new_directory  
       MDX_Tool -j -f my_new_bank.syx -d directory_with_VCEDs  
       MDX_Tool --info --file=my_dx_file.syx  
  ```
  
Split and Join parameters expect non-corrupted files as input (headerless files are accepted).  

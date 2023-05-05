The [wiki](https://github.com/BobanSpasic/MDX_Tool/wiki) here will contain more info on repairing the SysEx files. Do not "repair" your files without knowing what are you doing... or make backups before every operation.

Similear set of tools, but for the 4OP series, are located [here](https://github.com/BobanSpasic/MDX_Tool_4OP)

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
                                               Adds .x_corr as the file extension where x is the error code
                                               Lower 4 numbers are the count of min/max errors
                                               Higher numbers are the count of voices with values in normally unused bits
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
       -q               --quest              Rip MidiQuest SQL files (tested on MidiQuest 6)

       -f {filename}    --file={filename}    Input file (or output file for -j parameter)
       -d {directory}   --dir={directory}    Output directory for -s and -x parameters
                                               Input directory for -j parameter
                                               If it does not contain a drive letter, a sub-directory in
                                               the current directory will be created.

       -o               --overwrite          Overwrite source file at repairing or normalizing

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

   -t                 --movetree                Copy directory tree to the new location and move the diff. files
       -i {filename}      --incoming={filename} IncomingHasMore.dif list of the incoming collection
       -d {directory}     --dir={directory}     Root directory of the incoming file collection
       -o {directory}     --output={directory}  Output directory for the moved files

  Example usage:
       MDX_Collect -a -d MyCollection -r MyReports
       MDX_Collect -a -d NewFiles -r NewReports
       MDX_Collect -c -m MyCollection.hsl -i NewFiles.hsl -r MyReports
   ```
# MDX_Hash_Test
Test the internal hashing functions. There was a problem with different generated hash sums if the voice was from a VCED or from a VMEM file.

# Other stuff...
External library HashLib is used in both tools to calculate hash sums: https://github.com/Xor-el/HashLib4Pascal

Do not miss to take a look at the Wiki here. It is still in the development, but it will contain info on DX7 SysEx files, and how to repair the corrupted files.

echo "eid,num_reads" > "counts_18s_first_${1}.csv"

while read f; do

  eid=$( basename "$f" .cram | cut -d_ -f1 )
  echo "Sample: ${eid}"
  cram="/mnt/project/${f}"
  crai="${cram}.crai"
  bed="/mnt/project/data/beds/18s.bed"
  rdna_18s=$( samtools view -@ 32 -c -M -L "${bed}" -X "${cram}" "${crai}" )
  printf "%s,%s\n" ${eid} ${rdna_18s} >> "counts_18s_first_${1}.csv"

done < "/mnt/project/data/crams/crams_first_${1}.txt"

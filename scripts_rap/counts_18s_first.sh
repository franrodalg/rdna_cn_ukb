export project=$( dx pwd )

for i in {0..9}; do
        dx run swiss-army-knife/4.5.0 \
          -iin="${project}sc_cn/counts_18s_first_rap.sh" \
          -icmd="bash counts_18s_first_rap.sh '$i'" \
          --instance-type "mem1_ssd1_v2_x36" \
          --destination "${project}data/counts_18s/" \
          --priority low \
          -y --brief
done

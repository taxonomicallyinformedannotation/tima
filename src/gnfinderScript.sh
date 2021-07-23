echo "Submitting File original.tsv to gnfinder servers"
../bin/gnfinder find -c -l detect -s 3,4,5,6,8,9,11,12,118,128,132,147,148,150,155,158,163,164,165,167,169,174,175,179,180,187 -t "0" ../data/interim/organisms/original.tsv >../data/interim/organisms/cleaned.json
echo "The result is saved as cleaned.json"

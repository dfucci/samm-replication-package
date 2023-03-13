.PHONY : all clean

all : clean/*.csv results/dataset/answers.csv results/figure/*.png

clean :
	rm -f clean/*.csv
	rm -f results/dataset/*.*
	rm -f results/figure/*.png

clean/*.csv : cleaning.R data/survey/*.xlsx
	@Rscript cleaning.R

results/dataset/answers.csv : clean/*.csv
	@csvstack -d="," clean/*.csv > results/dataset/answers.csv

results/figure/*.png : results/dataset/answers.csv analysis.R
	@Rscript analysis.R

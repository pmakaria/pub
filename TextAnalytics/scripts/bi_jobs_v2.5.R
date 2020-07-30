#Load library
library(corpustools)
library(ggplot2) 
library(quanteda)


#get vacancies data
VACANCIES <- read.csv("Z:/ext_VMShared/BI_Jobs_v2/data/VACANCIES.csv", comment.char="#")
#VACANCIES$newdt<-as.POSIXct(VACANCIES$DATE)

#create a list of tools, frorm an external file
tools_list <- scan("Z:/ext_VMShared/BI_Jobs_v2/data/tools.txt", what="", sep="\n",skip = 1)


#create a "mapping" tools dictionary
tools_dict <- read.delim("Z:/ext_VMShared/BI_Jobs_v2/data/tools_dict.txt")

#create a list of hardskills, frorm an external file
hs_list <- scan("Z:/ext_VMShared/BI_Jobs_v2/data/hard_skills.txt", what="", sep="\n",skip = 1)

#create a list of softskills, frorm an external file
ss_list <- scan("Z:/ext_VMShared/BI_Jobs_v2/data/soft_skills.txt", what="", sep="\n",skip = 1)


#create corpus for vacancies
tc = create_tcorpus(VACANCIES, doc_column = 'JOBID', text_columns = 'REQUIREMENTS')

#remove duplicate job ads, keep only latest one based on date
#tc$deduplicate(feature='token', date_col = 'newdt', similarity = 0.75, keep = 'last')
#tc$meta$doc_id

#basic preprocessing

# The basic preprocessing techniques can be performed on a tcorpus with the preprocess method. The main arguments are:
#   
# column: the name of the column to use as input. Default is “token”
# new_column: the name of the column in which to store the output. Default is “feature”
# lowercase: make text lowercase. Defaults to TRUE
# remove_punctuation: remove punctuation. Defaults to TRUE
# use_stemming: apply stemming. Defaults to FALSE
# remove_stopwords: removes stopwords (e.g., the, it, is). Defaults to FALSE
# language: the language used for stemming and stopword removal. Defaults to “english”

tc$preprocess( remove_stopwords=T, remove_numbers = T, ngrams=1, ngram_context = 'document', min_freq = 10)

#map terms according to dictionary file
tc$replace_dictionary(tools_dict, string_col='string',token_col='feature',code_col='code',sep=" ",verbose=T)



#head(tc$tokens,20)

#"tag" tokens as tools, if found within the tools list.
tools_df<-as.data.frame(tools_list)
colnames(tools_df)<-c("string")

hs_df<-as.data.frame(hs_list)
colnames(hs_df)<-c("string")

ss_df<-as.data.frame(ss_list)
colnames(ss_df)<-c("string")


tc$code_dictionary(ss_df,token_col = 'token', string_col = 'string',flatten_colloc=T, sep=" ",verbose=T)



g = semnet_window(subset(tc,!is.na(code_id)),'feature')
plot_semnet(g)


gb = backbone_filter(g, alpha = 0.1, max_vertices =40)


plot_semnet(gb)

#The similarity measure. Currently supports: "con_prob" (conditional probability), "con_prob_weighted", "cosine" similarity, 
#"count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an 
#undirected network, chi2 (chi-square score))
#Co-occurence is calcuated based on how often two tokens occured within the same document


# μεγεθος ειναι το ποσες φορες εμφανιζεται  ( relative frequency)
# χρώμα ειναι αυτά που εμφανιζονται τις περισσοτερες φορες μαζι  (clusters)
# οι συνδέσεις με τι άλλο εμφανιζεται μαζι ( cooccurence )
# και η απόσταση δειχνει το ποσο δυνατή είναι η συσχέτιση μεταξύ τους.( magnitude of association)

g = semnet(subset(tc,!is.na(code_id)), 'feature', context_level='document', measure = 'chi2')
g = semnet(subset(tc,!is.na(code_id)), 'feature', context_level='document', measure = 'con_prob')
g = semnet(subset(tc,!is.na(code_id)), 'feature', context_level='document', measure = 'con_prob_weighted')
g = semnet(subset(tc,!is.na(code_id)), 'feature', context_level='document', measure = 'cosine')
g = semnet(subset(tc,!is.na(code_id)), 'feature', context_level='document', measure = 'count_directed')


plot_semnet (g)



##example
text = c('BO Oracle SQL', 'SQL R hadoop sap c vbscript msaccess excel teradata qliksense', 'Apache spark','Python','Python','Python','Python','Python', 'SQL Python','SQL Python','SQL Python','SQL Python','SQL Python' )
tc = create_tcorpus(text, doc_id = c('a','b','c','d','e','f','g','h','i','j','k','l','m'), split_sentences = TRUE)


g = semnet(tc, 'token',context_level='document',  measure = 'chi2')
plot_semnet (g)
##example end

#check Connections only to a specific node

tc2=subset(tc,!is.na(code_id))

g = tc2$semnet('feature')

igraph::get.data.frame(g)
plot_semnet(g)
## only keep nodes directly connected to given node
g_ego = ego_semnet(g, 'kubernetes')
igraph::get.data.frame(g_ego)
plot_semnet(g_ego)



#print a wordcloud for terms connected to sql
fa = feature_associations(tc2, 'feature', query = 'sql')
plot(fa)

comp = compare_subset(tc2, feature='feature',
                      subset_meta_x = SearchTerms == "business intelligence")
plot(comp)

comp = compare_subset(tc2, feature='feature',
                      subset_meta_x = SearchTerms == "Analytics")
plot(comp)


comp = compare_subset(tc2, feature='feature',
                      subset_meta_x = Country == "CH")
plot(comp)

comp = compare_subset(tc2, feature='feature',
                      subset_meta_x = Country == "GR")
plot(comp)
####end


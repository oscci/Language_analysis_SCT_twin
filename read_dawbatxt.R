
#Reads in from text file created by DAWBA which has original symptom scores etc.
#Script by DVMB 9th Dec 2017

require(tidyverse)
mydir<-"~/Dropbox/ERCadvanced/project SCT analysis/DAWBA/"
myfile<-'dawba2spss20171208190354'
mydata = read.table(paste0(mydir,myfile,".txt"),header=TRUE,sep = "\t")  # read text file 

#dawbaID is wrongly assigned so colnames are all out by one
#Should be possible to fix this with row.names command but I have not succeeded
#So will try to shuffle data along.

for (i in length(colnames(mydata)):2){
  mydata[,i]<-mydata[,(i-1)]
}
mydata[,1]<-rownames(mydata)
rownames(mydata)<-c() #remove rownames

#Variable definitions from DAWBA (intended for SPSS) are appended at end of this file

#Read in codes and identify twins (all twin codes have 4+ characters)
#NB dawba_codes.csv created from master xls list
# Had to remove entries where ID was 'CARDIFF' and 'test'
mycodes<-read.csv(paste0(mydir,'dawba_codes.csv'),stringsAsFactors=FALSE)
colnames(mycodes)[2]<-'record_id' #for compatibility with Redcap
mycodes$twin<-0
w<-which(nchar(mycodes$record_id)>3)
mycodes[w,3]<-1

matchlist<-mycodes$ID.Number %in% mydata$dawbaID
shortcodes<-mycodes[matchlist,] #shortcodes just has cases found in mydata

twincodes<-filter(shortcodes,twin==1) #twins only
twindawbas<-twincodes$ID.Number
myf<-filter(mydata, dawbaID %in% twindawbas) #pick only these twin cases from mydata
twindone<-cbind(twincodes,myf) #bolt on the data from twincodes
#NB dawbaID is now represented twice but that provides a check everything is properly aligned

#Select data to write to Redcap
#First import the template from redcap
mytemplate<-read.csv(paste0(mydir,'twin_redcap_template.csv'),stringsAsFactors=FALSE)

matchtwin<-mytemplate$record_id %in% twindone$record_id
mytemplate<-mytemplate[matchtwin,] #N of 290 in mytemplate but 293 in twindone
#suggests we have DAWBA for 3 cases not in main redcap file
#Checked and it seemed we have two pairs given two codes; now deleted
#These are DAWBA codes 143498-143501
#Also one DAWBA code with no data and a twin ID (1028) that does not exist
#These are now corrected on dawba_codes.csv

#NB the serial order in template file will be different from twindone.
#This will not matter if we first paste the twindone order into the template.
mytemplate$record_id<-twindone$record_id
#Then we just need to select the correct columns to write to remaining template columns
#First block is DSM diagnoses
#asd_dsm_r1	adhd_comb_dsm_r1	adhd_hyp_dsm_r1	adhd_inatt_dsm_r1	generalized_anx_dsm_r1	social_anx_dsm_r1	separation_anx_dsm_r1	other_anx_dsm_r1	phobia_dsm_r1	depression_dsm_r1	opp_def_dsm_r1	conduct_dsm_r1	chronic_tic_dsm_r1	tourette_dsm_r1	anorexia_dsm_r1

mydsmcols<-c('dcpdd','dcadhdc','dcadhdh','dcadhdi','dcgena','dcsoph','dcsepa',
             'dcotanx','dcspph','dcmadep','dcodd','dccd','dctic','dctic','dceat')
mytemplate[,2:16]<-twindone[,mydsmcols]
mytemplate[,17:34]<-NA #no icd for twins
mytemplate[,36:49]<-twindone[,569:582] #CGAS and Hon

w<-is.na(mytemplate$cgas_r1) #I had not coded those at ceiling so need to add now
mytemplate[w,36]<-95 
mytemplate[w,37:49]<-0
mytemplate[,52:57]<-twindone[,10:15] #SQD scales

mytemplate[,59:78]<-twindone[,27:46] #sep anx symptoms
mytemplate[,79:99]<-twindone[,73:93] #soc anx symptoms

mytemplate$dawba_id<-twindone$dawbaID

#Now write template file so can import to redcap
mytemplate<-mytemplate[,c(1:16,36:100)] #remove unused ICD cols as they just create problems with NA

w<-which(mytemplate$asd_dsm_r1>-1) #find and remove any with NA - these were incomplete DAWBAs
mytemplate<-mytemplate[w,]
write.csv(mytemplate,paste0(mydir,'twin_from_dawba.csv'),row.names=FALSE)

#Full list of variable names and values follows
# variable labels dawbaID 'ID'.
# variable labels age 'Age'.
# variable labels gender 'Gender'.
# variable labels p1startdate 'Data last entered (Parent1)'.
# variable labels p1type 'Informant (Parent1)'.
# variable labels p1ebdtot 'SDQ: Total difficulties score (Parent1)'.
# variable labels p1emotion 'SDQ: Emotional symptoms score (Parent1)'.
# variable labels p1conduct 'SDQ: Conduct problems score (Parent1)'.
# variable labels p1hyper 'SDQ: Hyperactivity score (Parent1)'.
# variable labels p1peer 'SDQ: Peer problems score (Parent1)'.
# variable labels p1prosoc 'SDQ: Prosocial score (Parent1)'.
# variable labels p1impact 'SDQ: Impact score (Parent1)'.
# variable labels p1a1a 'Attached to: Mother (Parent1)'.
# variable labels p1a1b 'Attached to: Father (Parent1)'.
# variable labels p1a1c 'Attached to: Other mother figure (Parent1)'.
# variable labels p1a1d 'Attached to: Other father figure (Parent1)'.
# variable labels p1a1e 'Attached to: Grandparents (Parent1)'.
# variable labels p1a1f 'Attached to: Adult relatives (Parent1)'.
# variable labels p1a1g 'Attached to: Childminder, nanny, au pair (Parent1)'.
# variable labels p1a1h 'Attached to: Teachers (Parent1)'.
# variable labels p1a1i 'Attached to: Adult non-relatives (Parent1)'.
# variable labels p1a1k 'Attached to: Brother, sisters (Parent1)'.
# variable labels p1a1l 'Attached to: Friends (Parent1)'.
# variable labels p1a2 'SepA: Any concerns about separations? (Parent1)'.
# variable labels p1a3a 'SepA: Loss of, or harm to, AFs (Parent1)'.
# variable labels p1a3b 'SepA: Being taken away from AFs (Parent1)'.
# variable labels p1a3c 'SepA: Not wanting to go to school (Parent1)'.
# variable labels p1a3d 'SepA: Afraid of sleeping alone (Parent1)'.
# variable labels p1a3e 'SepA: Sleeps with or checks on AFs at night (Parent1)'.
# variable labels p1a3f 'SepA: Afraid of sleeping in a strange place (Parent1)'.
# variable labels p1a3g 'SepA: Afraid of being in a room alone (Parent1)'.
# variable labels p1a3h 'SepA: Afraid of being at home alone (Parent1)'.
# variable labels p1a3i 'SepA: Nightmares of separation (Parent1)'.
# variable labels p1a3j 'SepA: Somatic symptoms linked to separations (Parent1)'.
# variable labels p1a3k 'SepA: Anticipatory anxiety of separations (Parent1)'.
# variable labels p1a4 'SepA: Symptoms for at least 1 month (Parent1)'.
# variable labels p1a5 'SepA: Age of onset (Parent1)'.
# variable labels p1a6 'SepA: Distress (Parent1)'.
# variable labels p1a7a 'SepA: Impact on family life (Parent1)'.
# variable labels p1a7b 'SepA: Impact on friendships (Parent1)'.
# variable labels p1a7c 'SepA: Impact on learning (Parent1)'.
# variable labels p1a7d 'SepA: Impact on leisure (Parent1)'.
# variable labels p1a8 'SepA: Burden (Parent1)'.
# variable labels p1sepabandd 'Computer prediction: Separation anxiety (Parent1, DSM-IV)'.
# variable labels p1sepabandi 'Computer prediction: Separation Anxiety (Parent1, ICD-10)'.
# variable labels p1b1a 'Specific fear of: Animals (Parent1)'.
# variable labels p1b1b 'Specific fear of: Storms, thunder, heights (Parent1)'.
# variable labels p1b1c 'Specific fear of: Dark (Parent1)'.
# variable labels p1b1d 'Specific fear of: Loud noises (Parent1)'.
# variable labels p1b1e 'Specific fear of: Blood, infection, injury (Parent1)'.
# variable labels p1b1f 'Specific fear of: Dentists, doctors (Parent1)'.
# variable labels p1b1g 'Specific fear of: Vomiting, choking, diseases (Parent1)'.
# variable labels p1b1h 'Specific fear of: Types of transport (Parent1)'.
# variable labels p1b1i 'Specific fear of: Enclosed spaces (Parent1)'.
# variable labels p1b1j 'Specific fear of: Toilets (Parent1)'.
# variable labels p1b1k 'Specific fear of: People who look unusual (Parent1)'.
# variable labels p1b1l 'Specific fear of: Monsters etc (Parent1)'.
# variable labels p1b1m 'Specific fear of: Other things (Parent1)'.
# variable labels p1b2 'SpPh: Fear is a nuisance (Parent1)'.
# variable labels p1b3 'SpPh: Duration in months (Parent1)'.
# variable labels p1b4 'SpPh: Very upset when fear is triggered (Parent1)'.
# variable labels p1b5 'SpPh: Upset every time (Parent1)'.
# variable labels p1b6 'SpPh: How often is fear triggered? (Parent1)'.
# variable labels p1b7 'SpPh: Avoids the phobic stimulus (Parent1)'.
# variable labels p1b8 'SpPh: Avoidance interferes with daily life (Parent1)'.
# variable labels p1b9 'SpPh: Others thinks fear is excessive (Parent1)'.
# variable labels p1b10 'SpPh: Child thinks fear is excessive (Parent1)'.
# variable labels p1b11 'SpPh: Burden (Parent1)'.
# variable labels p1spphband 'Computer prediction: Specific Phobia (Parent1, DSM-IV & ICD-10)'.
# variable labels p1c1 'SoPh: Any concerns? (Parent1)'.
# variable labels p1c2a 'Anxious about: Meeting new people (Parent1)'.
# variable labels p1c2b 'Anxious about: Meeting a lot of people (Parent1)'.
# variable labels p1c2c 'Anxious about: Eating in front of others (Parent1)'.
# variable labels p1c2d 'Anxious about: Speaking in class (Parent1)'.
# variable labels p1c2e 'Anxious about: Reading aloud in front of others (Parent1)'.
# variable labels p1c2f 'Anxious about: Writing in front of others (Parent1)'.
# variable labels p1c3 'SoPh: Separation or social anxiety? (Parent1)'.
# variable labels p1c4 'SoPh: Frightened with adults/kids (Parent1)'.
# variable labels p1c5 'SoPh: Can socialise with familiar people (Parent1)'.
# variable labels p1c6 'SoPh: Due to fear of embarrassment (Parent1)'.
# variable labels p1c7 'SoPh: Due to delay in speech, writing, reading (Parent1)'.
# variable labels p1c8 'SoPh: Duration in months (Parent1)'.
# variable labels p1c9 'SoPh: Age of onset (Parent1)'.
# variable labels p1c10 'SoPh: Upset when social fear is triggered (Parent1)'.
# variable labels p1c11 'SoPh: How often social fear is triggered (Parent1)'.
# variable labels p1c12 'SoPh: Avoids relevant social situations (Parent1)'.
# variable labels p1c13 'SoPh: Avoidance interferes with daily life (Parent1)'.
# variable labels p1c14 'SoPh: Child thinks fear is excessive (Parent1)'.
# variable labels p1c15 'SoPh: Child upset to have social fears (Parent1)'.
# variable labels p1c16 'SoPh: Burden (Parent1)'.
# variable labels p1sophband 'Computer prediction: Social phobia (Parent1, DSM-IV & ICD-10)'.
# variable labels p1d1 'Panic attacks in last 4 weeks (Parent1)'.
# variable labels p1d2a 'Fear or avoidance of: Crowds (Parent1)'.
# variable labels p1d2b 'Fear or avoidance of: Public Places (Parent1)'.
# variable labels p1d2c 'Fear or avoidance of: Travelling alone (Parent1)'.
# variable labels p1d2d 'Fear or avoidance of: Being far from home (Parent1)'.
# variable labels p1d3 'Fear or avoidance is due to panic attacks (Parent1)'.
# variable labels p1panband 'Computer prediction: Panic disorder (Parent1, DSM-IV & ICD-10)'.
# variable labels p1agoband 'Computer prediction: Agoraphobia (Parent1, DSM-IV & ICD-10)'.
# variable labels p1f1 'OCD: Any concerns? (Parent1)'.
# variable labels p1f2a 'OCD: Excessive washing (Parent1)'.
# variable labels p1f2b 'OCD: Avoidance of contamination (Parent1)'.
# variable labels p1f2c 'OCD: Checking (Parent1)'.
# variable labels p1f2d 'OCD: Repetitive actions (Parent1)'.
# variable labels p1f2e 'OCD: Touching things or people (Parent1)'.
# variable labels p1f2f 'OCD: Ordering / symmetry (Parent1)'.
# variable labels p1f2g 'OCD: Counting / avoiding unlucky numbers (Parent1)'.
# variable labels p1f3 'OCD: Concern about contamination (Parent1)'.
# variable labels p1f4 'OCD: Concern about bad things happening (Parent1)'.
# variable labels p1f6 'OCD: Due to separation anxiety? (Parent1)'.
# variable labels p1f7 'OCD: Present daily for 2 weeks (Parent1)'.
# variable labels p1f8 'OCD: Rituals or obsessions >1 hour per day (Parent1)'.
# variable labels p1f9 'OCD: Insight that its excessive (Parent1)'.
# variable labels p1f10 'OCD: Reaction to rituals or obsessions (Parent1)'.
# variable labels p1f11 'OCD: Resistance (Parent1)'.
# variable labels p1f12a 'OCD: Impact on family life (Parent1)'.
# variable labels p1f12b 'OCD: Impact on friendships (Parent1)'.
# variable labels p1f12c 'OCD: Impact on learning (Parent1)'.
# variable labels p1f12d 'OCD: Impact on leisure (Parent1)'.
# variable labels p1f13 'OCD: Burden (Parent1)'.
# variable labels p1ocdband 'Computer prediction: OCD (Parent1, DSM-IV & ICD-10)'.
# variable labels p1z1 'BDD: Concerns about appearance (Parent1)'.
# variable labels p1z2a 'BDD: Skin condition (Parent1)'.
# variable labels p1z2b 'BDD: Skin colour (Parent1)'.
# variable labels p1z2c 'BDD: Hair colour or condition (Parent1)'.
# variable labels p1z2d 'BDD: Muscle bulk (Parent1)'.
# variable labels p1z2e 'BDD: Body shape or size (Parent1)'.
# variable labels p1z2f 'BDD: Facial features (Parent1)'.
# variable labels p1z2g 'BDD: Other body part (Parent1)'.
# variable labels p1z2h 'BDD: Asymmetry (Parent1)'.
# variable labels p1z2i 'BDD: Other aspect of appearance (Parent1)'.
# variable labels p1z3 'BDD: Age of onset (Parent1)'.
# variable labels p1z4a 'BDD: Repeatedly compares self with others (Parent1)'.
# variable labels p1z4b 'BDD: Repeatedly checking own appearance (Parent1)'.
# variable labels p1z4c 'BDD: Putting a lot of effort into improving appearance (Parent1)'.
# variable labels p1z4d 'BDD: Hiding appearance (Parent1)'.
# variable labels p1z4e 'BDD: Seeking reassurance about appearance (Parent1)'.
# variable labels p1z4f 'BDD: Efforts to improve muscle mass or body shape (Parent1)'.
# variable labels p1z4g 'BDD: Cosmetic surgery requested or used (Parent1)'.
# variable labels p1z5a 'BDD: Time spent worrying about appearance (Parent1)'.
# variable labels p1z5b 'BDD: Time spent hiding or improving appearance (Parent1)'.
# variable labels p1z6 'BDD: Others thinks worry is excessive (Parent1)'.
# variable labels p1z7 'BDD: Subject thinks worry is excessive (Parent1)'.
# variable labels p1z8 'BDD: Distress (Parent1)'.
# variable labels p1z9a 'BDD: Impact on family life (Parent1)'.
# variable labels p1z9b 'BDD: Impact on friendships (Parent1)'.
# variable labels p1z9c 'BDD: Impact on learning (Parent1)'.
# variable labels p1z9d 'BDD: Impact on leisure (Parent1)'.
# variable labels p1z10 'BDD: Burden (Parent1)'.
# variable labels p1bddtot 'BDD: Total BDD score (Parent1)'.
# variable labels p1z2sum 'BDD: preoccupation score (Parent1)'.
# variable labels p1z4sum 'BDD: repetition score (Parent1)'.
# variable labels p1bddimp 'BDD: Total BDD impact (Parent1)'.
# variable labels p1g2 'GenA: Ever worries? (Parent1)'.
# variable labels p1g2a 'GenA: Specific or generalised? (Parent1)'.
# variable labels p1g3 'GenA: Excessive worry (Parent1)'.
# variable labels p1g4a 'GenA: Past behaviour (Parent1)'.
# variable labels p1g4b 'GenA: School work/examinations (Parent1)'.
# variable labels p1g4c 'GenA: Disasters/accidents (Parent1)'.
# variable labels p1g4d 'GenA: Own health (Parent1)'.
# variable labels p1g4e 'GenA: Bad things happening to others (Parent1)'.
# variable labels p1g4f 'GenA: The future (Parent1)'.
# variable labels p1g4g 'GenA: Making and keeping friends (Parent1)'.
# variable labels p1g4h 'GenA: Death and dying (Parent1)'.
# variable labels p1g4i 'GenA: Being bullied and teased (Parent1)'.
# variable labels p1g4j 'GenA: Own appearance or weight (Parent1)'.
# variable labels p1g4k 'GenA: Other worries (Parent1)'.
# variable labels p1g6 'GenA: Worried on most days in last 6 months (Parent1)'.
# variable labels p1g7 'GenA: Worry difficult to control (Parent1)'.
# variable labels p1g8a 'GenA: Restlessness (Parent1)'.
# variable labels p1g8b 'GenA: Fatigue (Parent1)'.
# variable labels p1g8c 'GenA: Poor concentration (Parent1)'.
# variable labels p1g8d 'GenA: Irritable (Parent1)'.
# variable labels p1g8e 'GenA: Muscular tension (Parent1)'.
# variable labels p1g8f 'GenA: Insomnia (Parent1)'.
# variable labels p1g9 'GenA: Distress (Parent1)'.
# variable labels p1g10a 'GenA: Impact on family life (Parent1)'.
# variable labels p1g10b 'GenA: Impact on friendships (Parent1)'.
# variable labels p1g10c 'GenA: Impact on learning (Parent1)'.
# variable labels p1g10d 'GenA: Impact on leisure (Parent1)'.
# variable labels p1g11 'GenA: Burden (Parent1)'.
# variable labels p1genaband 'Computer prediction: Generalised anxiety (Parent1, DSM-IV & ICD-10)'.
# variable labels p1y1 'DMDD: Frequency of irritable/angry mood (Parent1)'.
# variable labels p1y2 'DMDD: Frequency of outbursts (Parent1)'.
# variable labels p1y3a 'DMDD: Slamming doors (Parent1)'.
# variable labels p1y3b 'DMDD: Shouting (Parent1)'.
# variable labels p1y3c 'DMDD: Swearing (Parent1)'.
# variable labels p1y3d 'DMDD: Saying mean things to others (Parent1)'.
# variable labels p1y3e 'DMDD: Saying negative things about himself (Parent1)'.
# variable labels p1y3f 'DMDD: Physical aggression to others (Parent1)'.
# variable labels p1y3g 'DMDD: Deliberate self-harm (Parent1)'.
# variable labels p1y3h 'DMDD: Breaking things (Parent1)'.
# variable labels p1y4a 'DMDD: At home (Parent1)'.
# variable labels p1y4b 'DMDD: In the classroom (Parent1)'.
# variable labels p1y4c 'DMDD: With peers (Parent1)'.
# variable labels p1y5 'DMDD: His outbursts have recognizable triggers (Parent1)'.
# variable labels p1y6 'DMDD: His outbursts are easily triggered (Parent1)'.
# variable labels p1y7 'DMDD: Longest outburst-free gap in the last year (Parent1)'.
# variable labels p1y8 'DMDD: Easily irritated (Parent1)'.
# variable labels p1y9 'DMDD: Intense irritability (Parent1)'.
# variable labels p1y10 'DMDD: Long duration of irritability (Parent1)'.
# variable labels p1y11 'DMDD: Irritability evident to others (Parent1)'.
# variable labels p1y12a 'DMDD: At home (Parent1)'.
# variable labels p1y12b 'DMDD: In the classroom (Parent1)'.
# variable labels p1y12c 'DMDD: With peers (Parent1)'.
# variable labels p1y13 'DMDD: Angry weeks i.e. irritable most of the day, nearly every day (Parent1)'.
# variable labels p1y14 'DMDD: Proportion of angry weeks (past year) (Parent1)'.
# variable labels p1y15 'DMDD: Longest gap between angry weeks (past year) (Parent1)'.
# variable labels p1y16 'DMDD: Respondent concerned about irritability or temper outbursts (Parent1)'.
# variable labels p1y17 'DMDD: Age of onset (Parent1)'.
# variable labels p1y18 'DMDD: Distress (Parent1)'.
# variable labels p1y19a 'DMDD: Impact on family life (Parent1)'.
# variable labels p1y19b 'DMDD: Impact on friendships (Parent1)'.
# variable labels p1y19c 'DMDD: Impact on learning (Parent1)'.
# variable labels p1y19d 'DMDD: Impact on leisure (Parent1)'.
# variable labels p1y20 'DMDD: Burden (Parent1)'.
# variable labels p1dmddtot 'DMDD: Total DMDD score (Parent1)'.
# variable labels p1irrit 'DMDD: irritability score (Parent1)'.
# variable labels p1temper 'DMDD: temper score (Parent1)'.
# variable labels p1dmddimp 'DMDD: Total DMDD impact (Parent1)'.
# variable labels p1h1 'Dep: Sad (Parent1)'.
# variable labels p1h2 'Dep: Miserable daily (Parent1)'.
# variable labels p1h3 'Dep: Miserable most of day (Parent1)'.
# variable labels p1h4 'Dep: Can be cheered up (Parent1)'.
# variable labels p1h5 'Dep: Duration (weeks) (Parent1)'.
# variable labels p1h7 'Dep: Irritable (Parent1)'.
# variable labels p1h8 'Dep: Irritable daily (Parent1)'.
# variable labels p1h9 'Dep: Irritable most of day (Parent1)'.
# variable labels p1h10 'Dep: Improved by friends (Parent1)'.
# variable labels p1h11 'Dep: Duration (weeks) (Parent1)'.
# variable labels p1h13 'Dep: Loss of interest (Parent1)'.
# variable labels p1h14 'Dep: No interest daily (Parent1)'.
# variable labels p1h15 'Dep: No interest for most of the day (Parent1)'.
# variable labels p1h16 'Dep: Duration (weeks) (Parent1)'.
# variable labels p1h17 'Dep: Coincided with irritability/misery (Parent1)'.
# variable labels p1h18a 'Dep: Tired/no energy (Parent1)'.
# variable labels p1h18b 'Dep: Changed appetite (Parent1)'.
# variable labels p1h18c 'Dep: Weight loss/gain (Parent1)'.
# variable labels p1h18d 'Dep: Insomnia (Parent1)'.
# variable labels p1h18e 'Dep: Hypersomnia (Parent1)'.
# variable labels p1h18f 'Dep: Agitation (Parent1)'.
# variable labels p1h18g 'Dep: Feels worthless, guilty (Parent1)'.
# variable labels p1h18h 'Dep: Poor concentration (Parent1)'.
# variable labels p1h18i 'Dep: Thoughts of death (Parent1)'.
# variable labels p1h18j 'Dep: Recent talk of DSH (Parent1)'.
# variable labels p1h18k 'Dep: DSH recently (Parent1)'.
# variable labels p1h18l 'Dep: DSH ever (Parent1)'.
# variable labels p1h19 'Dep: Distress (Parent1)'.
# variable labels p1h20a 'Dep: Impact on family life (Parent1)'.
# variable labels p1h20b 'Dep: Impact on friendships (Parent1)'.
# variable labels p1h20c 'Dep: Impact on learning (Parent1)'.
# variable labels p1h20d 'Dep: Impact on leisure (Parent1)'.
# variable labels p1h21 'Dep: Burden (Parent1)'.
# variable labels p1depband 'Computer prediction: Depression (Parent1, DSM-IV & ICD-10)'.
# variable labels p1s1 'Bip: Unstable mood (Parent1)'.
# variable labels p1s2a 'Bip: Rapid (Parent1)'.
# variable labels p1s2b 'Bip: Marked (Parent1)'.
# variable labels p1s2c 'Bip: Unpredictable (Parent1)'.
# variable labels p1s2d 'Bip: Frequent (Parent1)'.
# variable labels p1s3 'Bip: Duration (Parent1)'.
# variable labels p1s4 'Bip: Elevated mood (Parent1)'.
# variable labels p1s5a 'Bip: Cheerful (Parent1)'.
# variable labels p1s5b 'Bip: Talking fast (Parent1)'.
# variable labels p1s5c 'Bip: Active (Parent1)'.
# variable labels p1s5d 'Bip: Achieving more (Parent1)'.
# variable labels p1s5e 'Bip: Noisy (Parent1)'.
# variable labels p1s5f 'Bip: Spends money fast (Parent1)'.
# variable labels p1s5g 'Bip: Needs less sleep (Parent1)'.
# variable labels p1s5h 'Bip: Restless (Parent1)'.
# variable labels p1s5i 'Bip: Over-sexed (Parent1)'.
# variable labels p1s5j 'Bip: Frequent changes of plan (Parent1)'.
# variable labels p1s5k 'Bip: Full of energy (Parent1)'.
# variable labels p1s5l 'Bip: Talks to strangers (Parent1)'.
# variable labels p1s5m 'Bip: Excitable (Parent1)'.
# variable labels p1s5n 'Bip: Less concerned about trouble (Parent1)'.
# variable labels p1s5o 'Bip: Invades personal space (Parent1)'.
# variable labels p1s5p 'Bip: Over-confident (Parent1)'.
# variable labels p1s5q 'Bip: Takes serious risks (Parent1)'.
# variable labels p1s5r 'Bip: Jokes and laughs more (Parent1)'.
# variable labels p1s5s 'Bip: More outgoing (Parent1)'.
# variable labels p1s5t 'Bip: Irritable (Parent1)'.
# variable labels p1s5u 'Bip: Distractible (Parent1)'.
# variable labels p1s5v 'Bip: Disinhibited (Parent1)'.
# variable labels p1s5w 'Bip: Poor concentration (Parent1)'.
# variable labels p1s5x 'Bip: Too bossy (Parent1)'.
# variable labels p1s5y 'Bip: Appearance neglected (Parent1)'.
# variable labels p1s5z 'Bip: Rapid shifts of topic (Parent1)'.
# variable labels p1s6a 'Bip: Visual hallucinations (Parent1)'.
# variable labels p1s6b 'Bip: Auditory hallucinations (Parent1)'.
# variable labels p1s6c 'Bip: Special powers (Parent1)'.
# variable labels p1s6d 'Bip: Regret afterwards (Parent1)'.
# variable labels p1s7 'Bip: Length of episode (Parent1)'.
# variable labels p1s8 'Bip: Mixed affective state (Parent1)'.
# variable labels p1s9 'Bip: High in the last 4 weeks (Parent1)'.
# variable labels p1s10 'Bip: Longest episode in the last 4 weeks (Parent1)'.
# variable labels p1s11a 'Bip: Impact on family life (Parent1)'.
# variable labels p1s11b 'Bip: Impact on friendships (Parent1)'.
# variable labels p1s11c 'Bip: Impact on learning (Parent1)'.
# variable labels p1s11d 'Bip: Impact on leisure (Parent1)'.
# variable labels p1s12 'Bip: Burden (Parent1)'.
# variable labels p1j1 'ADHD: Any concerns? (Parent1)'.
# variable labels p1j2a 'ADHD: Fidgets (Parent1)'.
# variable labels p1j2b 'ADHD: Cant remain seated (Parent1)'.
# variable labels p1j2c 'ADHD: Runs or climbs when shouldnt (Parent1)'.
# variable labels p1j2d 'ADHD: Cant play quietly (Parent1)'.
# variable labels p1j2e 'ADHD: Cant calm down (Parent1)'.
# variable labels p1j3a 'ADHD: Blurts out answers (Parent1)'.
# variable labels p1j3b 'ADHD: Cant wait for a turn (Parent1)'.
# variable labels p1j3c 'ADHD: Butts into conversations or games (Parent1)'.
# variable labels p1j3d 'ADHD: Unstoppable talk (Parent1)'.
# variable labels p1j4a 'ADHD: Careless mistakes/inattentive (Parent1)'.
# variable labels p1j4b 'ADHD: Loses interest (Parent1)'.
# variable labels p1j4c 'ADHD: Doesnt listen (Parent1)'.
# variable labels p1j4d 'ADHD: Doesnt finish task (Parent1)'.
# variable labels p1j4e 'ADHD: Poor self organisation (Parent1)'.
# variable labels p1j4f 'ADHD: Avoids tasks needing thought (Parent1)'.
# variable labels p1j4g 'ADHD: Loses things (Parent1)'.
# variable labels p1j4h 'ADHD: Distractible (Parent1)'.
# variable labels p1j4i 'ADHD: Forgetful (Parent1)'.
# variable labels p1j5a 'ADHD: Teacher complains of overactivity (Parent1)'.
# variable labels p1j5b 'ADHD: Teacher complains of poor attention (Parent1)'.
# variable labels p1j5c 'ADHD: Teacher complains of impulsivity (Parent1)'.
# variable labels p1j6 'ADHD: Present for at least 6 months (Parent1)'.
# variable labels p1j7 'ADHD: Age of onset (Parent1)'.
# variable labels p1j8 'ADHD: Distress (Parent1)'.
# variable labels p1j9a 'ADHD: Impact on family life (Parent1)'.
# variable labels p1j9b 'ADHD: Impact on friendships (Parent1)'.
# variable labels p1j9c 'ADHD: Impact on learning (Parent1)'.
# variable labels p1j9d 'ADHD: Impact on leisure (Parent1)'.
# variable labels p1j10 'ADHD: Burden (Parent1)'.
# variable labels p1adhdbandd 'Computer prediction: ADHD (Parent1, DSM-IV)'.
# variable labels p1adhdbandi 'Computer prediction: Hyperkinesis (Parent1, ICD-10)'.
# variable labels p1k1 'ODD: As difficult as other kids? (Parent1)'.
# variable labels p1k2a 'ODD: Temper outbursts (Parent1)'.
# variable labels p1k2b 'ODD: Argues with adults (Parent1)'.
# variable labels p1k2c 'ODD: Ignores rules/disobedient (Parent1)'.
# variable labels p1k2d 'ODD: Deliberately annoys others (Parent1)'.
# variable labels p1k2e 'ODD: Blames others for own acts (Parent1)'.
# variable labels p1k2f 'ODD: Easily annoyed (Parent1)'.
# variable labels p1k2g 'ODD: Angry and resentful (Parent1)'.
# variable labels p1k2h 'ODD: Spiteful (Parent1)'.
# variable labels p1k2i 'ODD: Vindictive (Parent1)'.
# variable labels p1k3 'ODD: Teacher has similar complaints (Parent1)'.
# variable labels p1k4 'ODD: Present at least 6 months (Parent1)'.
# variable labels p1k5 'ODD: Age of onset (Parent1)'.
# variable labels p1k6a 'ODD: Impact on family life (Parent1)'.
# variable labels p1k6b 'ODD: Impact on friendships (Parent1)'.
# variable labels p1k6c 'ODD: Impact on learning (Parent1)'.
# variable labels p1k6d 'ODD: Impact on leisure (Parent1)'.
# variable labels p1k7 'ODD: Burden (Parent1)'.
# variable labels p1oddband 'Computer prediction: Oppositional defiant (Parent1, DSM-IV & ICD-10)'.
# variable labels p1k8a 'CD: Lies (Parent1)'.
# variable labels p1k8b 'CD: Fights (Parent1)'.
# variable labels p1k8c 'CD: Bullies (Parent1)'.
# variable labels p1k8d 'CD: Stays out (Parent1)'.
# variable labels p1k8e 'CD: Steals (Parent1)'.
# variable labels p1k8f 'CD: Runs away (Parent1)'.
# variable labels p1k8g 'CD: Truants (Parent1)'.
# variable labels p1k9 'CD: Truanted<13 (Parent1)'.
# variable labels p1k10a 'CD: Uses weapons (Parent1)'.
# variable labels p1k10b 'CD: Cruel to people (Parent1)'.
# variable labels p1k10c 'CD: Cruel to animals (Parent1)'.
# variable labels p1k10d 'CD: Fire setting (Parent1)'.
# variable labels p1k10e 'CD: Destructive (Parent1)'.
# variable labels p1k10f 'CD: Mugging (Parent1)'.
# variable labels p1k10g 'CD: Forced sex (Parent1)'.
# variable labels p1k10h 'CD: Breaks in (Parent1)'.
# variable labels p1k11aa 'CD: Present>6 months (Parent1)'.
# variable labels p1k11 'CD: Teacher complained (Parent1)'.
# variable labels p1k11a 'CD: Police contact (Parent1)'.
# variable labels p1k12a 'CD: Impact on family life (Parent1)'.
# variable labels p1k12b 'CD: Impact on friendships (Parent1)'.
# variable labels p1k12c 'CD: Impact on learning (Parent1)'.
# variable labels p1k12d 'CD: Impact on leisure (Parent1)'.
# variable labels p1k13 'CD: Burden (Parent1)'.
# variable labels p1cdband 'Computer prediction: Conduct disorder (Parent1, DSM-IV & ICD-10)'.
# variable labels p1sas1 'Social Aptitude Scale: Can laugh around with others (Parent1)'.
# variable labels p1sas2 'Social Aptitude Scale: Easy to chat with (Parent1)'.
# variable labels p1sas3 'Social Aptitude Scale: Flexible, can compromise (Parent1)'.
# variable labels p1sas4 'Social Aptitude Scale: Can defuse tense situations (Parent1)'.
# variable labels p1sas5 'Social Aptitude Scale: Good loser (Parent1)'.
# variable labels p1sas6 'Social Aptitude Scale: Puts others at ease (Parent1)'.
# variable labels p1sas7 'Social Aptitude Scale: Can tell what others think and feel (Parent1)'.
# variable labels p1sas8 'Social Aptitude Scale: Apologizes, puts things right (Parent1)'.
# variable labels p1sas9 'Social Aptitude Scale: Leads without seeming bossy (Parent1)'.
# variable labels p1sas10 'Social Aptitude Scale: Recognizes what is socially appropriate (Parent1)'.
# variable labels p1sastot 'Social Aptitude Scale: Total score (Parent1)'.
# variable labels p1fr1 'ASD: Difficulty making friends (Parent1)'.
# variable labels p1fr2 'ASD: Difficulty keeping friends (Parent1)'.
# variable labels p1fr3 'ASD: Number of friends he fairly often spends time with (Parent1)'.
# variable labels p1fr4 'ASD: Shares interests with friends (Parent1)'.
# variable labels p1fr5 'ASD: Does things jointly with friends (Parent1)'.
# variable labels p1fr6 'ASD: Confides in friends (Parent1)'.
# variable labels p1r1 'ASD: General reasoning and school work at present (Parent1)'.
# variable labels p1r2 'ASD: Current mental age (Parent1)'.
# variable labels p1r3 'ASD: Language expression and comprehension (Parent1)'.
# variable labels p1r4 'ASD: Current language age (Parent1)'.
# variable labels p1r5 'ASD: Good at getting round language difficulties (Parent1)'.
# variable labels p1r6a 'Serious concerns in the first 3 years: about speech (Parent1)'.
# variable labels p1r6b 'Serious concerns in the first 3 years: abour social interaction (Parent1)'.
# variable labels p1r6c 'Serious concerns in the first 3 years: about pretend play (Parent1)'.
# variable labels p1r6d 'Serious concerns in the first 3 years: about rituals / stereotypies (Parent1)'.
# variable labels p1r6e 'ASD: Concern in first 3 years about general mental development (Parent1)'.
# variable labels p1r7 'ASD: Continuing difficulties in any of these areas (Parent1)'.
# variable labels p1r8 'ASD: Words before aged 2 (Parent1)'.
# variable labels p1r9 'ASD: Phrases before aged 3 (Parent1)'.
# variable labels p1r10 'ASD: Restricted use of nonverbal gestures as a toddler and young child (Parent1)'.
# variable labels p1r11 'ASD: Enjoying simple social games as a toddler (Parent1)'.
# variable labels p1r12 'ASD: Sharing enjoyment, interests or achievements at age 4 (Parent1)'.
# variable labels p1r13 'ASD: Repetitive play, e.g. turning light switches on and off (ever) (Parent1)'.
# variable labels p1r14 'ASD: Very interested in unusual aspects of toys and other things (ever) (Parent1)'.
# variable labels p1r15 'ASD: Regularly taking part in imaginative play (ever) (Parent1)'.
# variable labels p1r16 'ASD: Adjusts play for older or younger children (Parent1)'.
# variable labels p1r17 'ASD: Difficulty taking turns, sharing, cooperating (Parent1)'.
# variable labels p1r18 'ASD: Any obsessions? (Parent1)'.
# variable labels p1r19 'ASD: Unusual topic (Parent1)'.
# variable labels p1r20 'ASD: Dominating his life (Parent1)'.
# variable labels p1r21 'ASD: Dominating his conversation (Parent1)'.
# variable labels p1r22 'ASD: Interfering with getting on with other things (Parent1)'.
# variable labels p1r24 'ASD: Good at starting conversations with others (Parent1)'.
# variable labels p1r25 'ASD: Good at sustaining conversations started by others (Parent1)'.
# variable labels p1r26 'ASD: Interested in chatting about other peoples interests (Parent1)'.
# variable labels p1r27 'ASD: Adjusts conversation for formal and informal situations (Parent1)'.
# variable labels p1r28 'ASD: Others find it hard to read his tone of voice and facial expressions (Parent1)'.
# variable labels p1r29 'ASD: Finds it hard to read others tone of voice and facial expressions (Parent1)'.
# variable labels p1r30 'ASD: Abnormal eye contact at some age (too much, too little, wrong type) (Parent1)'.
# variable labels p1r31 'ASD: A lot of echoing (ever) (Parent1)'.
# variable labels p1r32 'ASD: Repetitive questioning (ever) (Parent1)'.
# variable labels p1r33 'ASD: Repetitive clichés (ever) (Parent1)'.
# variable labels p1r34 'ASD: Strong or unusual routines (ever) (Parent1)'.
# variable labels p1r36 'ASD: Very upset by change in routine (ever) (Parent1)'.
# variable labels p1r37 'ASD: A lot of flapping (ever) (Parent1)'.
# variable labels p1r38 'ASD: Parental concern about language, play, flexibility etc (Parent1)'.
# variable labels p1r39 'ASD: Distress (Parent1)'.
# variable labels p1r40a 'ASD: Impact on family life (Parent1)'.
# variable labels p1r40b 'ASD: Impact on friendships (Parent1)'.
# variable labels p1r40c 'ASD: Impact on learning (Parent1)'.
# variable labels p1r40d 'ASD: Impact on leisure (Parent1)'.
# variable labels p1r41 'ASD: Burden (Parent1)'.
# variable labels p1r42 'ASD: Always there or sudden onset (with regression)? (Parent1)'.
# variable labels p1r43 'ASD: Age when change took place (Parent1)'.
# variable labels p1asdband 'Computer prediction: PDD/Autism (Parent1, DSM-IV & ICD-10)'.
# variable labels p1dev1 'Dev: Stereotypic actions (Parent1)'.
# variable labels p1dev2a 'Dev: Rocking back and forth (Parent1)'.
# variable labels p1dev2b 'Dev: Head nodding (Parent1)'.
# variable labels p1dev2c 'Dev: Flapping or hand twisting (Parent1)'.
# variable labels p1dev2d 'Dev: Fluttering fingers in front of face (Parent1)'.
# variable labels p1dev2e 'Dev: Waving an object in front of face (Parent1)'.
# variable labels p1dev2f 'Dev: Picking at skin (Parent1)'.
# variable labels p1dev2g 'Dev: Head banging (Parent1)'.
# variable labels p1dev2h 'Dev: Biting lips, hands etc. (Parent1)'.
# variable labels p1dev2i 'Dev: Eye poking (Parent1)'.
# variable labels p1dev2j 'Dev: Other repetitive actions (Parent1)'.
# variable labels p1dev3 'Dev: Time spent on repetitive activities (Parent1)'.
# variable labels p1dev4 'Dev: Age of onset of repetitive activities (Parent1)'.
# variable labels p1dev5 'Dev: Distress (Parent1)'.
# variable labels p1dev6a 'Dev: Impact on family life (Parent1)'.
# variable labels p1dev6b 'Dev: Impact on friendships (Parent1)'.
# variable labels p1dev6c 'Dev: Impact on learning (Parent1)'.
# variable labels p1dev6d 'Dev: Impact on leisure (Parent1)'.
# variable labels p1dev7 'Dev: Burden (Parent1)'.
# variable labels p1dev9a 'Dev: Rough behaviour (Parent1)'.
# variable labels p1dev9b 'Dev: Attacks others (Parent1)'.
# variable labels p1dev9c 'Dev: Reduced sexual inhibition (Parent1)'.
# variable labels p1dev9d 'Dev: Underactivity (Parent1)'.
# variable labels p1dev9e 'Dev: Too noisy (Parent1)'.
# variable labels p1n1a '+ve: Generous (Parent1)'.
# variable labels p1n1b '+ve: Lively (Parent1)'.
# variable labels p1n1c '+ve: Keen to learn (Parent1)'.
# variable labels p1n1d '+ve: Affectionate (Parent1)'.
# variable labels p1n1e '+ve: Reliable and responsible (Parent1)'.
# variable labels p1n1f '+ve: Easygoing (Parent1)'.
# variable labels p1n1g '+ve: Good fun, good sense of humour (Parent1)'.
# variable labels p1n1h '+ve: Interested in many things (Parent1)'.
# variable labels p1n1i '+ve: Caring, kind-hearted (Parent1)'.
# variable labels p1n1j '+ve: Bounces back quickly after setbacks (Parent1)'.
# variable labels p1n1k '+ve: Grateful, appreciative (Parent1)'.
# variable labels p1n1l '+ve: Independent (Parent1)'.
# variable labels p1n2a '+ve: Helps around the home (Parent1)'.
# variable labels p1n2b '+ve: Gets on well with the rest of the family (Parent1)'.
# variable labels p1n2c '+ve: Does homework without reminding (Parent1)'.
# variable labels p1n2d '+ve: Creative activities (Parent1)'.
# variable labels p1n2e '+ve: Likes to be involved in family activities (Parent1)'.
# variable labels p1n2f '+ve: Takes care of appearance (Parent1)'.
# variable labels p1n2g '+ve: Good at school work (Parent1)'.
# variable labels p1n2h '+ve: Polite (Parent1)'.
# variable labels p1n2i '+ve: Good at sport (Parent1)'.
# variable labels p1n2j '+ve: Keeps bedroom tidy (Parent1)'.
# variable labels p1n2k '+ve: Good with friends (Parent1)'.
# variable labels p1n2l '+ve: Well behaved (Parent1)'.
# variable labels dcany 'Any disorder (Clinical rating, DSM-IV)'.
# variable labels dcemot 'Emotional (Clinical rating, DSM-IV)'.
# variable labels dcsepa 'Separation anxiety (Clinical rating, DSM-IV)'.
# variable labels dcspph 'Specific Phobia (Clinical rating, DSM-IV)'.
# variable labels dcsoph 'Social phobia (Clinical rating, DSM-IV)'.
# variable labels dcpanic 'Panic disorder (Clinical rating, DSM-IV)'.
# variable labels dcagor 'Agoraphobia (Clinical rating, DSM-IV)'.
# variable labels dcptsd 'PTSD (Clinical rating, DSM-IV)'.
# variable labels dcocd 'OCD (Clinical rating, DSM-IV)'.
# variable labels dcgena 'Generalised anxiety (Clinical rating, DSM-IV)'.
# variable labels dcotanx 'Other anxiety (Clinical rating, DSM-IV)'.
# variable labels dcdmdd 'DMDD (Clinical rating, DSM-IV)'.
# variable labels dcmadep 'Major depression (Clinical rating, DSM-IV)'.
# variable labels dcotdep 'Other depression (Clinical rating, DSM-IV)'.
# variable labels dcundif 'Undiff anx/dep (Clinical rating, DSM-IV)'.
# variable labels dcmania 'Mania/bipolar (Clinical rating, DSM-IV)'.
# variable labels dcanyso 'Social (Clinical rating, DSM-IV)'.
# variable labels dcmutis 'Selective mutism (Clinical rating, DSM-IV)'.
# variable labels dcdisat 'Attach disorder (disin) (Clinical rating, DSM-IV)'.
# variable labels dcinhat 'Attach disorder (inhib) (Clinical rating, DSM-IV)'.
# variable labels dcothat 'Attach disorder (other) (Clinical rating, DSM-IV)'.
# variable labels dcanyhk 'ADHD (Clinical rating, DSM-IV)'.
# variable labels dcadhdc 'ADHD combined (Clinical rating, DSM-IV)'.
# variable labels dcadhdi 'ADHD inattentive (Clinical rating, DSM-IV)'.
# variable labels dcadhdh 'ADHD hyp-imp (Clinical rating, DSM-IV)'.
# variable labels dcadhdo 'Other hyperactivity (Clinical rating, DSM-IV)'.
# variable labels dcanycd 'Conduct/Oppositional (Clinical rating, DSM-IV)'.
# variable labels dcodd 'Oppositional defiant (Clinical rating, DSM-IV)'.
# variable labels dccd 'Conduct disorder (Clinical rating, DSM-IV)'.
# variable labels dcothcd 'Other disruptive (Clinical rating, DSM-IV)'.
# variable labels dcother 'Other (Clinical rating, DSM-IV)'.
# variable labels dcpdd 'PDD/Autism (Clinical rating, DSM-IV)'.
# variable labels dctic 'Tic disorder (Clinical rating, DSM-IV)'.
# variable labels dceat 'Eating disorder (Clinical rating, DSM-IV)'.
# variable labels dcpsych 'Psychosis (Clinical rating, DSM-IV)'.
# variable labels dcstere 'Stereotypic (Clinical rating, DSM-IV)'.
# variable labels dcototh 'Any other (Clinical rating, DSM-IV)'.
# variable labels icany 'Any disorder (Clinical rating, ICD-10)'.
# variable labels icemot 'Emotional (Clinical rating, ICD-10)'.
# variable labels icsepa 'Separation Anxiety (Clinical rating, ICD-10)'.
# variable labels icspph 'Specific phobia (Clinical rating, ICD-10)'.
# variable labels icsoph 'Social phobia (Clinical rating, ICD-10)'.
# variable labels icpanic 'Panic disorder (Clinical rating, ICD-10)'.
# variable labels icagor 'Agoraphobia (Clinical rating, ICD-10)'.
# variable labels icptsd 'PTSD (Clinical rating, ICD-10)'.
# variable labels icocd 'OCD (Clinical rating, ICD-10)'.
# variable labels icgena 'Generalised anxiety (Clinical rating, ICD-10)'.
# variable labels icotanx 'Other anxiety (Clinical rating, ICD-10)'.
# variable labels icmadep 'Depressive episode (Clinical rating, ICD-10)'.
# variable labels icotdep 'Other depression (Clinical rating, ICD-10)'.
# variable labels icundif 'Undiff anx/dep (Clinical rating, ICD-10)'.
# variable labels icmania 'Mania/bipolar (Clinical rating, ICD-10)'.
# variable labels icanyso 'Social (Clinical rating, ICD-10)'.
# variable labels icmutis 'Selective mutism (Clinical rating, ICD-10)'.
# variable labels icdisat 'Attach disorder (disin) (Clinical rating, ICD-10)'.
# variable labels icreact 'Attach disorder (react) (Clinical rating, ICD-10)'.
# variable labels icothat 'Attach disorder (other) (Clinical rating, ICD-10)'.
# variable labels icanyhk 'Hyperactivity (Clinical rating, ICD-10)'.
# variable labels ichyper 'Hyperkinesis (Clinical rating, ICD-10)'.
# variable labels icothk 'Other hyperactivity (Clinical rating, ICD-10)'.
# variable labels icanycd 'Conduct/Oppositional (Clinical rating, ICD-10)'.
# variable labels icodd 'Oppositional defiant (Clinical rating, ICD-10)'.
# variable labels iccdfam 'CD confined to family (Clinical rating, ICD-10)'.
# variable labels icunsoc 'Unsocialised CD (Clinical rating, ICD-10)'.
# variable labels icsoccd 'Socialised CD (Clinical rating, ICD-10)'.
# variable labels icothcd 'Other CD (Clinical rating, ICD-10)'.
# variable labels icother 'Other (Clinical rating, ICD-10)'.
# variable labels icpdd 'PDD/Autism (Clinical rating, ICD-10)'.
# variable labels ictic 'Tic disorder (Clinical rating, ICD-10)'.
# variable labels iceat 'Eating disorder (Clinical rating, ICD-10)'.
# variable labels icpsych 'Psychosis (Clinical rating, ICD-10)'.
# variable labels icstere 'Stereotyped (Clinical rating, ICD-10)'.
# variable labels icototh 'Any other (Clinical rating, ICD-10)'.
# variable labels ratername 'Rated by (Clinical rating, DSM-IV & ICD-10)'.
# variable labels ratedate 'Rated on (Clinical rating, DSM-IV & ICD-10)'.
# variable labels cgas 'C-GAS: Total score (Clinical rating)'.
# variable labels hon1 'HoNOSCA: Disruptive, antisocial or aggressive behaviour (Clinical rating)'.
# variable labels hon2 'HoNOSCA: Overactivity, attention and concentration (Clinical rating)'.
# variable labels hon3 'HoNOSCA: Non accidental self injury (Clinical rating)'.
# variable labels hon4 'HoNOSCA: Alcohol, substance/solvent misuse (Clinical rating)'.
# variable labels hon5 'HoNOSCA: Scholastic or language skills (Clinical rating)'.
# variable labels hon6 'HoNOSCA: Physical illness or disability problems (Clinical rating)'.
# variable labels hon7 'HoNOSCA: Hallucinations and delusions (Clinical rating)'.
# variable labels hon8 'HoNOSCA: Non-organic somatic symptoms (Clinical rating)'.
# variable labels hon9 'HoNOSCA: Emotional and related symptoms (Clinical rating)'.
# variable labels hon10 'HoNOSCA: Peer relationships (Clinical rating)'.
# variable labels hon11 'HoNOSCA: Self care and independence (Clinical rating)'.
# variable labels hon12 'HoNOSCA: Family life and relationships (Clinical rating)'.
# variable labels hon13 'HoNOSCA: Poor school attendance (Clinical rating)'.
# variable labels hontot 'HoNOSCA: Total score (Clinical rating)'.
# execute.
# 
# value labels gender 1 'Male' 2 'Female'.
# value labels p1type 1 'Parent' 2 'Mother' 3 'Father' 4 'Both parents' 5 'Stepmother' 6 'Stepfather' 7 'Foster mother' 8 'Foster father' 9 'Grandparent' 10 'Other relative' 11 'Residential care worker' 12 'Friend' 13 'Partner' 14 'Sibling' 15 'Son or daughter' 16 'Other non-relative'.
# value labels p1a1a 0 'No' 1 'Yes'.
# value labels p1a1b 0 'No' 1 'Yes'.
# value labels p1a1c 0 'No' 1 'Yes'.
# value labels p1a1d 0 'No' 1 'Yes'.
# value labels p1a1e 0 'No' 1 'Yes'.
# value labels p1a1f 0 'No' 1 'Yes'.
# value labels p1a1g 0 'No' 1 'Yes'.
# value labels p1a1h 0 'No' 1 'Yes'.
# value labels p1a1i 0 'No' 1 'Yes'.
# value labels p1a1k 0 'No' 1 'Yes'.
# value labels p1a1l 0 'No' 1 'Yes'.
# value labels p1a2 0 'No' 1 'Yes'.
# value labels p1a3a 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3b 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3c 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3d 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3e 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3f 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3g 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3h 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3i 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3j 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a3k 0 'No more than others, or doesnt apply' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1a4 0 'No' 1 'Yes'.
# value labels p1a5 -1 '?'.
# value labels p1a6 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1a7a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1a7b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1a7c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1a7d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1a8 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1sepabandd 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1sepabandi 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1b1a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1i 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1j 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1k 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1l 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b1m 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b2 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1b3 0 'Less than 1 month' 1 '1-5 months' 2 '6 months or more'.
# value labels p1b4 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b5 0 'No' 1 'Yes'.
# value labels p1b6 0 'Every now and then' 1 'Most weeks' 2 'Most days' 3 'Many times a day'.
# value labels p1b7 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b8 0 'No' 1 'A little' 2 'A lot'.
# value labels p1b9 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1b10 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1b11 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1spphband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1c1 0 'No' 1 'Yes'.
# value labels p1c2a 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1c2b 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1c2c 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1c2d 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1c2e 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1c2f 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1c3 0 'Mostly fine in social situations as long as key adults are around' 1 'Social fears are marked even when key adults are around'.
# value labels p1c4 0 'Just with adults' 1 'Just with children' 2 'With both adults and children '.
# value labels p1c5 0 'No' 1 'Yes'.
# value labels p1c6 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1c7 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1c8 0 'Less than 1 month' 1 '1-5 months' 2 '6 months or more'.
# value labels p1c9 -1 '?'.
# value labels p1c10 0 'No' 1 'A little' 2 'A lot'.
# value labels p1c11 0 'Every now and then' 1 'Most weeks' 2 'Most days' 3 'Many times a day'.
# value labels p1c12 0 'No' 1 'A little' 2 'A lot'.
# value labels p1c13 0 'No' 1 'A little' 2 'A lot'.
# value labels p1c14 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1c15 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1c16 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1sophband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1d1 0 'No' 1 'Yes'.
# value labels p1d2a 0 'No, or doesnt apply' 1 'Yes'.
# value labels p1d2b 0 'No, or doesnt apply' 1 'Yes'.
# value labels p1d2c 0 'No, or doesnt apply' 1 'Yes'.
# value labels p1d2d 0 'No, or doesnt apply' 1 'Yes'.
# value labels p1d3 0 'No' 1 'Yes'.
# value labels p1panband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1agoband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1f1 0 'No' 1 'Yes'.
# value labels p1f2a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f2b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f2c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f2d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f2e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f2f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f2g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f3 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f4 0 'No' 1 'A little' 2 'A lot'.
# value labels p1f6 0 'Part of separation anxiety' 1 'A problem in its own right'.
# value labels p1f7 0 'No' 1 'Yes'.
# value labels p1f8 0 'No' 1 'Yes'.
# value labels p1f9 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1f10 0 'No, he enjoys them' 1 'Neutral - he neither enjoys them nor becomes upset' 2 'They upset him a little' 3 'They upset him a lot'.
# value labels p1f11 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1f12a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1f12b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1f12c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1f12d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1f13 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1ocdband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1z1 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z2i 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z3 -1 '?'.
# value labels p1z4a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z4b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z4c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z4d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z4e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z4f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z4g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1z5a 0 'Little or no time' 1 'Less than an hour' 2 'About an hour' 3 'A few hours' 4 'Many hours'.
# value labels p1z5b 0 'Little or no time' 1 'Less than an hour' 2 'About an hour' 3 'A few hours' 4 'Many hours'.
# value labels p1z6 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1z7 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1z8 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1z9a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1z9b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1z9c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1z9d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1z10 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1g2 0 'No' 1 'Yes'.
# value labels p1g2a 0 'No, he just has a few specific worries' 1 'Yes, he worries in general'.
# value labels p1g3 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1g4a 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4b 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4c 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4d 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4e 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4f 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4g 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4h 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4i 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4j 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g4k 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1g6 0 'No' 1 'Yes'.
# value labels p1g7 0 'No' 1 'Yes'.
# value labels p1g8a 0 'No' 1 'Yes, but not on most days' 2 'Yes, happens more days than not'.
# value labels p1g8b 0 'No' 1 'Yes, but not on most days' 2 'Yes, happens more days than not'.
# value labels p1g8c 0 'No' 1 'Yes, but not on most days' 2 'Yes, happens more days than not'.
# value labels p1g8d 0 'No' 1 'Yes, but not on most days' 2 'Yes, happens more days than not'.
# value labels p1g8e 0 'No' 1 'Yes, but not on most days' 2 'Yes, happens more days than not'.
# value labels p1g8f 0 'No' 1 'Yes, but not on most days' 2 'Yes, happens more days than not'.
# value labels p1g9 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1g10a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1g10b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1g10c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1g10d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1g11 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1genaband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1y1 0 'Never' 1 'Occasionally' 2 'Once or twice a week' 3 'Three or more times a week' 4 'Every day'.
# value labels p1y2 0 'Never' 1 'Occasionally' 2 'Once or twice a week' 3 'Three or more times a week' 4 'Every day'.
# value labels p1y3a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y3h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y4a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y4b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y4c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y5 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1y6 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y7 0 'Less than a day' 1 'Less than a week' 2 'Less than a month' 3 '1-3 months' 4 'More than 3 months'.
# value labels p1y8 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y9 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y10 0 'No more than a few minutes' 1 'Less than an hour' 2 'A few hours' 3 'Most or all of the day'.
# value labels p1y11 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1y12a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y12b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y12c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y13 0 'No' 1 'Yes'.
# value labels p1y14 0 'Well under half of them' 1 'About half of them' 2 'Well over half of them' 3 'All or nearly all of them'.
# value labels p1y15 0 'Less than a month' 1 '1-3 months' 2 'More than 3 months'.
# value labels p1y16 0 'No' 1 'A little' 2 'A lot'.
# value labels p1y17 -1 '?'.
# value labels p1y18 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1y19a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1y19b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1y19c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1y19d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1y20 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1h1 0 'No' 1 'Yes'.
# value labels p1h2 0 'No' 1 'Yes'.
# value labels p1h3 0 'No' 1 'Yes'.
# value labels p1h4 0 'Easily' 1 'With difficulty/only briefly' 2 'Not at all'.
# value labels p1h5 0 'Less than 2 weeks' 1 '2 weeks or more'.
# value labels p1h7 0 'No' 1 'Yes'.
# value labels p1h8 0 'No' 1 'Yes'.
# value labels p1h9 0 'No' 1 'Yes'.
# value labels p1h10 0 'Easily' 1 'With difficulty/only briefly' 2 'Not at all'.
# value labels p1h11 0 'Less than 2 weeks' 1 '2 weeks or more'.
# value labels p1h13 0 'No' 1 'Yes'.
# value labels p1h14 0 'No' 1 'Yes'.
# value labels p1h15 0 'No' 1 'Yes'.
# value labels p1h16 0 'Less than 2 weeks' 1 '2 weeks or more'.
# value labels p1h17 0 'No' 1 'Yes'.
# value labels p1h18a 0 'No' 1 'Yes'.
# value labels p1h18b 0 'No' 1 'Yes'.
# value labels p1h18c 0 'No' 1 'Yes'.
# value labels p1h18d 0 'No' 1 'Yes'.
# value labels p1h18e 0 'No' 1 'Yes'.
# value labels p1h18f 0 'No' 1 'Yes'.
# value labels p1h18g 0 'No' 1 'Yes'.
# value labels p1h18h 0 'No' 1 'Yes'.
# value labels p1h18i 0 'No' 1 'Yes'.
# value labels p1h18j 0 'No' 1 'Yes'.
# value labels p1h18k 0 'No' 1 'Yes'.
# value labels p1h18l 0 'No' 1 'Yes'.
# value labels p1h19 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1h20a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1h20b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1h20c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1h20d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1h21 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1depband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1s1 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s2a 0 'No' 1 'Yes'.
# value labels p1s2b 0 'No' 1 'Yes'.
# value labels p1s2c 0 'No' 1 'Yes'.
# value labels p1s2d 0 'No' 1 'Yes'.
# value labels p1s3 0 'Minutes' 1 'Hours' 2 'Most of the day, or longer'.
# value labels p1s4 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5i 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5j 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5k 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5l 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5m 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5n 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5o 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5p 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5q 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5r 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5s 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5t 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5u 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5v 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5w 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5x 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5y 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s5z 0 'No' 1 'A little' 2 'A lot'.
# value labels p1s6a 0 'No' 1 'Yes'.
# value labels p1s6b 0 'No' 1 'Yes'.
# value labels p1s6c 0 'No' 1 'Yes'.
# value labels p1s6d 0 'No' 1 'Yes'.
# value labels p1s7 0 'Less than an hour' 1 'Less than a day' 2 '1-3 days' 3 '4-6 days' 4 'One week or more'.
# value labels p1s8 0 'No' 1 'Yes'.
# value labels p1s9 0 'No' 1 'Yes'.
# value labels p1s10 0 'Less than 4 days' 1 '4-6 days' 2 'One week or more'.
# value labels p1s11a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1s11b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1s11c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1s11d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1s12 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1j1 0 'No' 1 'Yes'.
# value labels p1j2a 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j2b 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j2c 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j2d 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j2e 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j3a 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j3b 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j3c 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j3d 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4a 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4b 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4c 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4d 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4e 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4f 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4g 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4h 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j4i 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1j5a 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1j5b 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1j5c 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1j6 0 'No' 1 'Yes'.
# value labels p1j7 -1 '?'.
# value labels p1j8 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1j9a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1j9b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1j9c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1j9d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1j10 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1adhdbandd 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1adhdbandi 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1k1 0 'Less difficult or troublesome than average' 1 'About average' 2 'More difficult or troublesome than average'.
# value labels p1k2a 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2b 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2c 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2d 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2e 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2f 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2g 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2h 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k2i 0 'No more than others' 1 'A little more than others' 2 'A lot more than others'.
# value labels p1k3 0 'No, or doesnt apply' 1 'A little' 2 'A lot'.
# value labels p1k4 0 'No' 1 'Yes'.
# value labels p1k5 -1 '?'.
# value labels p1k6a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k6b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k6c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k6d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k7 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1oddband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1k8a 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k8b 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k8c 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k8d 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k8e 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k8f 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k8g 0 'No' 1 'Perhaps' 2 'True of the last 6 months' 3 'Was true 7-12 months ago, but not since'.
# value labels p1k9 0 'No' 1 'Yes'.
# value labels p1k10a 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10b 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10c 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10d 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10e 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10f 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10g 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k10h 0 'No' 1 'True of the last 6 months' 2 'Was true 7-12 months ago, but not since'.
# value labels p1k11aa 0 'No' 1 'Yes'.
# value labels p1k11 0 'No, or doesnt apply' 1 'Yes'.
# value labels p1k11a 0 'No' 1 'Yes'.
# value labels p1k12a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k12b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k12c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k12d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1k13 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1cdband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1sas1 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas2 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas3 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas4 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas5 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas6 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas7 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas8 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas9 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1sas10 0 'A lot worse than average' 1 'A bit worse than average' 2 'About average' 3 'A bit better than average' 4 'A lot better than average'.
# value labels p1fr1 0 'Finds it harder than average' 1 'About average' 2 'Finds it easier than average'.
# value labels p1fr2 0 'Finds it harder than average' 1 'About average' 2 'Finds it easier than average'.
# value labels p1fr3 0 'none' 1 'one' 2 '2-4' 3 '5-9' 4 '10+'.
# value labels p1fr4 0 'No' 1 'A little' 2 'A lot'.
# value labels p1fr5 0 'No' 1 'A little' 2 'A lot'.
# value labels p1fr6 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1r1 0 'Ahead' 1 'Average' 2 'Behind'.
# value labels p1r2 -1 '?'.
# value labels p1r3 0 'Ahead' 1 'Average' 2 'Behind'.
# value labels p1r4 -1 '?'.
# value labels p1r5 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r6a 0 'No' 1 'Yes'.
# value labels p1r6b 0 'No' 1 'Yes'.
# value labels p1r6c 0 'No' 1 'Yes'.
# value labels p1r6d 0 'No' 1 'Yes'.
# value labels p1r6e 0 'No' 1 'Yes'.
# value labels p1r7 0 'Completely cleared up' 1 'Some continuing problems'.
# value labels p1r8 0 'No' 1 'Yes'.
# value labels p1r9 0 'No' 1 'Yes'.
# value labels p1r10 0 'About the same or more' 1 'A little less' 2 'A lot less'.
# value labels p1r11 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r12 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r13 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r14 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r15 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r16 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r17 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r18 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r19 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r20 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r21 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r22 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r24 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r25 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r26 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r27 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r28 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r29 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r30 0 'No' 1 'Perhaps' 2 'Definitely'.
# value labels p1r31 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r32 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r33 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r34 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r36 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r37 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r38 0 'No' 1 'A little' 2 'A lot'.
# value labels p1r39 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1r40a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1r40b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1r40c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1r40d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1r41 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1r42 0 'Always there to some extent' 1 'Sudden change'.
# value labels p1r43 -1 '?'.
# value labels p1asdband 0 '<0.1%' 1 '~0.5%' 2 '~3%' 3 '~15%' 4 '~50%' 5 '70%+'.
# value labels p1dev1 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2i 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev2j 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev3 0 'No more than a few minutes' 1 'Less than an hour' 2 'A few hours' 3 'Most or all of the day'.
# value labels p1dev4 -1 '?'.
# value labels p1dev5 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1dev6a 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1dev6b 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1dev6c 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1dev6d 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1dev7 0 'Not at all' 1 'A little' 2 'A medium amount' 3 'A great deal'.
# value labels p1dev9a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev9b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev9c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev9d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1dev9e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1i 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1j 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1k 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n1l 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2a 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2b 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2c 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2d 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2e 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2f 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2g 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2h 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2i 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2j 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2k 0 'No' 1 'A little' 2 'A lot'.
# value labels p1n2l 0 'No' 1 'A little' 2 'A lot'.
# value labels dcany 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcemot 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcsepa 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcspph 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcsoph 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcpanic 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcagor 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcptsd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcocd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcgena 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcotanx 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcdmdd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcmadep 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcotdep 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcundif 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcmania 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcanyso 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcmutis 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcdisat 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcinhat 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcothat 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcanyhk 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcadhdc 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcadhdi 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcadhdh 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcadhdo 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcanycd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcodd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dccd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcothcd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcother 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcpdd 0 'No' 1 '?Unsure?' 2 'Autism' 3 'Asperger' 4 'Autism spectrum disorder, NOS'.
# value labels dctic 0 'No' 1 '?Unsure?' 2 'Tourette' 3 'Chronic tic' 4 'Other tic'.
# value labels dceat 0 'No' 1 '?Unsure?' 2 'Anorexia nervosa' 3 'Bulimia nervosa' 4 'Eating disorder, NOS' 5 'Binge-Eating Disorder'.
# value labels dcpsych 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcstere 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels dcototh 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icany 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icemot 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icsepa 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icspph 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icsoph 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icpanic 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icagor 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icptsd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icocd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icgena 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icotanx 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icmadep 0 'No' 1 '?Unsure?' 2 'Mild depressive episode' 3 'Moderate depressive episode' 4 'Severe depressive episode'.
# value labels icotdep 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icundif 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icmania 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icanyso 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icmutis 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icdisat 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icreact 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icothat 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icanyhk 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels ichyper 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icothk 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icanycd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icodd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels iccdfam 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icunsoc 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icsoccd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icothcd 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icother 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icpdd 0 'No' 1 '?Unsure?' 2 'Autism' 3 'Asperger' 4 'Autism spectrum disorder, NOS'.
# value labels ictic 0 'No' 1 '?Unsure?' 2 'Tourette' 3 'Chronic tic' 4 'Other tic'.
# value labels iceat 0 'No' 1 '?Unsure?' 2 'Anorexia nervosa' 3 'Bulimia nervosa' 4 'Eating disorder, NOS' 5 'Binge-Eating Disorder'.
# value labels icpsych 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icstere 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels icototh 0 'No' 1 '?Unsure?' 2 'Yes'.
# value labels hon1 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon2 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon3 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon4 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon5 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon6 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon7 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon8 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon9 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon10 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon11 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon12 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
# value labels hon13 0 'No' 1 'Minor' 2 'Mild' 3 'Moderate' 4 'Severe' 9 '?'.
data=read.csv("survey_data.csv")
summary(data)
genders=data$What.Gender.Do.You.Identify.As.
genders
races=data$How.Would.You.Identify.Your.Ethnicity.
races
graduates=data$Are.You.A.Graduate.Student
second=data$What.Best.Describes.Your.Second.Position.Out.of.Grad.School.2
first=data$What.Best.Describes.Your.First.Position.Out.of.Grad.School
third=data$What.Best.Describes.Your.Third.Position.Out.of.Grad.School.1
fourth=data$What.Best.Describes.Your.Fourth.Position.Out.of.Grad.School.1
fifth=data$What.Best.Describes.Your.Fifth.Position.Out.of.Grad.School.1
df=data.frame(genders,races,graduates,first,second,third,fourth,fifth)
data_matrix=as.matrix(df)
a[1:length(genders)]=paste(data_matrix[1:length(genders),1],"-",data_matrix[1:length(genders),2],"-",data_matrix[1:length(genders),3],"-",data_matrix[1:length(genders),4],"-",data_matrix[1:length(genders),5],"-",data_matrix[1:length(genders),6],"-",data_matrix[1:length(genders),7])
table(a)
df=data.frame(table(a))
write.table(df,file="output_third.txt")

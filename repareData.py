file= open("house_data7.csv","r")
lines=file.readlines()
file.close()

fileToWrite=open("data.csv","w")
iterator=0
for i in lines:
    #replacing , with . - numbers should be 3.14
    i=i.replace(',','.')
    #replacing ; with , - columns should be read easly
    i=i.replace(";",",")
    # adding row index
    fileToWrite.writelines(i)
    iterator+=1
file.close()


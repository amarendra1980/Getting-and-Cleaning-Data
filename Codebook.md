# Codebook
Codebook was generated on  "2017-06-07 12:25:27"

## Variable list and Description
Variable Name | Variable Description
------------- | --------------------
subject | ID of the subject who performed the activity for each window sample. Its range is from 1 to 30
activityName | Activity name
tBodyAccmeanX | Body Accelerator mean on X axis
tBodyAccmeanY | Body Accelerator mean on Y axis
tBodyAccmeanZ | Body Accelerator mean on Z axis
tBodyAccstdX  | Body Accelerator standard deviation on X axis
tBodyAccstdY  | Body Accelerator standard deviation on Y axis
tBodyAccstdZ  | Body Accelerator standard deviation on Z axis
tGravityAccmeanX | Gravity Accelerator mean on X axis
tGravityAccmeanY | Gravity Accelerator mean on Y axis
tGravityAccmeanZ | Gravity Accelerator mean on Z axis
tGravityAccstdX  | Gravity Accelerator Std deviation on X axis
tGravityAccstdY  | Gravity Accelerator Std deviation on Y axis
tGravityAccstdZ  | Gravity Accelerator Std deviation on Z axis
tBodyAccJerkmeanX | Body accelerator jerk mean on X axis
tBodyAccJerkmeanY | Body accelerator jerk mean on Y axis
tBodyAccJerkmeanZ | Body accelerator jerk mean on Z axis
tBodyAccJerkstdX  | Body accelerator jerk Std deviation on X axis
tBodyAccJerkstdY  | Body accelerator jerk Std deviation on Y axis
tBodyAccJerkstdZ  | Body accelerator jerk Std deviation on Z axis
tBodyGyromeanX    | Body gyroscope mean on X axis
tBodyGyromeanY    | Body gyroscope mean on Y axis
tBodyGyromeanZ    | Body gyroscope mean on Z axis
tBodyGyrostdX     | Body gyroscope Std deviation on X axis
tBodyGyrostdY     | Body gyroscope Std deviation on Y axis
tBodyGyrostdZ     | Body gyroscope Std deviation on Z axis
tBodyGyroJerkmeanX | Body gyroscope jerk mean on X axis
tBodyGyroJerkmeanY | Body gyroscope jerk mean on Y axis
tBodyGyroJerkmeanZ | Body gyroscope jerk mean on Z axis
tBodyGyroJerkstdX  | Body gyroscope jerk Std deviation on X axis
tBodyGyroJerkstdY  | Body gyroscope jerk Std deviation on Y axis
tBodyGyroJerkstdZ  | Body gyroscope jerk Std deviation on Z axis
tBodyAccMagmean    | Body accelerator mag mean
tBodyAccMagstd     | Body accelerator mag Std deviation
tGravityAccMagmean | Gravity accelerator mag mean
tGravityAccMagstd  | Gravity accelerator mag Std deviation
tBodyAccJerkMagmean | Body accelerator jerk mag mean
tBodyAccJerkMagstd  | Body accelerator jerk mag Std deviation
tBodyGyroMagmean   | Body gyroscope mag mean
tBodyGyroMagstd    | Body gyroscope standard deviation
tBodyGyroJerkMagmean | Body gyroscope jerk mag mean
tBodyGyroJerkMagstd | Body gyroscope jerk mag Std deviation
fBodyAccmeanX    | fBody accelerator mean on X axis
fBodyAccmeanY    | fBody accelerator mean on Y axis
fBodyAccmeanZ    | fBody accelerator mean on Z axis
fBodyAccstdX     | fBody accelerator Std deviation on X axis
fBodyAccstdY     | fBody accelerator Std deviation on Y axis
fBodyAccstdZ     | fBody accelerator Std deviation on Z axis
fBodyAccJerkmeanX | fBody accelerator jerk mean on X axis
fBodyAccJerkmeanY | fBody accelerator jerk mean on Y axis
fBodyAccJerkmeanZ | fBody accelerator jerk mean on Z axis
fBodyAccJerkstdX  | fBody accelerator jerk Std deviation on X axis
fBodyAccJerkstdY  | fBody accelerator jerk Std deviation on Y axis
fBodyAccJerkstdZ  | fBody accelerator jerk Std deviation on Z axis
fBodyGyromeanX    | fBody gyroscope mean on X axis
fBodyGyromeanY    | fBody gyroscope mean on Y axis
fBodyGyromeanZ    | fBody gyroscope mean on Z axis
fBodyGyrostdX     | fBody gyroscope std deviation on X axis
fBodyGyrostdY     | fBody gyroscope std deviation on Y axis
fBodyGyrostdZ     | fBody gyroscope std deviation on Z axis
fBodyAccMagmean   | fBody accelerator Mag mean
fBodyAccMagstd    | fBody accelerator Mag Std deviation
fBodyBodyAccJerkMagmean | fBody accelerator jerk mag mean
fBodyBodyAccJerkMagstd	| fBody accelerator jerk mag Std deviation
fBodyBodyGyroMagmean	| fBody gyroscope mag mean
fBodyBodyGyroMagstd 	| fBody gyroscope mag Std deviation
fBodyBodyGyroJerkMagmean | fBody gyroscope jerk mag mean
fBodyBodyGyroJerkMagstd	 | fBody gyroscopr jerk mag Std deviation

## Dataset structure
*'data.frame'    180 obs. of  68 variables:
* $ subject                 : int  1 1 1 1 1 1 2 2 2 2 ...
* $ activityName            : Factor w/ 6 levels "LAYING","SITTING",..: 1 2 3 4 5 6 1 2 3 4 ...
* $ tBodyAccmeanX           : num  0.222 0.261 0.279 0.277 0.289 ...
* $ tBodyAccmeanY           : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
* $ tBodyAccmeanZ           : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
* $ tBodyAccstdX            : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
* $ tBodyAccstdY            : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
* $ tBodyAccstdZ            : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...
* $ tGravityAccmeanX        : num  -0.249 0.832 0.943 0.935 0.932 ...
* $ tGravityAccmeanY        : num  0.706 0.204 -0.273 -0.282 -0.267 ...
* $ tGravityAccmeanZ        : num  0.4458 0.332 0.0135 -0.0681 -0.0621 ...
* $ tGravityAccstdX         : num  -0.897 -0.968 -0.994 -0.977 -0.951 ...
* $ tGravityAccstdY         : num  -0.908 -0.936 -0.981 -0.971 -0.937 ...
* $ tGravityAccstdZ         : num  -0.852 -0.949 -0.976 -0.948 -0.896 ...
* $ tBodyAccJerkmeanX       : num  0.0811 0.0775 0.0754 0.074 0.0542 ...
* $ tBodyAccJerkmeanY       : num  0.003838 -0.000619 0.007976 0.028272 0.02965 ...
* $ tBodyAccJerkmeanZ       : num  0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...
* $ tBodyAccJerkstdX        : num  -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...
* $ tBodyAccJerkstdY        : num  -0.924 -0.981 -0.986 0.067 -0.102 ...
* $ tBodyAccJerkstdZ        : num  -0.955 -0.988 -0.992 -0.503 -0.346 ...
* $ tBodyGyromeanX          : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
* $ tBodyGyromeanY          : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
* $ tBodyGyromeanZ          : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...
* $ tBodyGyrostdX           : num  -0.874 -0.977 -0.987 -0.474 -0.458 ...
* $ tBodyGyrostdY           : num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
* $ tBodyGyrostdZ           : num  -0.908 -0.941 -0.981 -0.344 -0.125 ...
* $ tBodyGyroJerkmeanX      : num  -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...
* $ tBodyGyroJerkmeanY      : num  -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...
* $ tBodyGyroJerkmeanZ      : num  -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...
* $ tBodyGyroJerkstdX       : num  -0.919 -0.992 -0.993 -0.207 -0.487 ...
* $ tBodyGyroJerkstdY       : num  -0.968 -0.99 -0.995 -0.304 -0.239 ...
* $ tBodyGyroJerkstdZ       : num  -0.958 -0.988 -0.992 -0.404 -0.269 ...
* $ tBodyAccMagmean         : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
* $ tBodyAccMagstd          : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
* $ tGravityAccMagmean      : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
* $ tGravityAccMagstd       : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
* $ tBodyAccJerkMagmean     : num  -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...
* $ tBodyAccJerkMagstd      : num  -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...
* $ tBodyGyroMagmean        : num  -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...
* $ tBodyGyroMagstd         : num  -0.819 -0.935 -0.979 -0.187 -0.226 ...
* $ tBodyGyroJerkMagmean    : num  -0.963 -0.992 -0.995 -0.299 -0.295 ...
* $ tBodyGyroJerkMagstd     : num  -0.936 -0.988 -0.995 -0.325 -0.307 ...
* $ fBodyAccmeanX           : num  -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...
* $ fBodyAccmeanY           : num  -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...
* $ fBodyAccmeanZ           : num  -0.883 -0.959 -0.985 -0.332 -0.226 ...
* $ fBodyAccstdX            : num  -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...
* $ fBodyAccstdY            : num  -0.834 -0.917 -0.972 0.056 -0.113 ...
* $ fBodyAccstdZ            : num  -0.813 -0.934 -0.978 -0.28 -0.298 ...
* $ fBodyAccJerkmeanX       : num  -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...
* $ fBodyAccJerkmeanY       : num  -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...
* $ fBodyAccJerkmeanZ       : num  -0.948 -0.986 -0.991 -0.469 -0.288 ...
* $ fBodyAccJerkstdX        : num  -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...
* $ fBodyAccJerkstdY        : num  -0.932 -0.983 -0.987 0.107 -0.135 ...
* $ fBodyAccJerkstdZ        : num  -0.961 -0.988 -0.992 -0.535 -0.402 ...
* $ fBodyGyromeanX          : num  -0.85 -0.976 -0.986 -0.339 -0.352 ...
* $ fBodyGyromeanY          : num  -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...
* $ fBodyGyromeanZ          : num  -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...
* $ fBodyGyrostdX           : num  -0.882 -0.978 -0.987 -0.517 -0.495 ...
* $ fBodyGyrostdY           : num  -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...
* $ fBodyGyrostdZ           : num  -0.917 -0.944 -0.982 -0.437 -0.238 ...
* $ fBodyAccMagmean         : num  -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...
* $ fBodyAccMagstd          : num  -0.798 -0.928 -0.982 -0.398 -0.187 ...
* $ fBodyBodyAccJerkMagmean : num  -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...
* $ fBodyBodyAccJerkMagstd  : num  -0.922 -0.982 -0.993 -0.103 -0.104 ...
* $ fBodyBodyGyroMagmean    : num  -0.862 -0.958 -0.985 -0.199 -0.186 ...
* $ fBodyBodyGyroMagstd     : num  -0.824 -0.932 -0.978 -0.321 -0.398 ...
* $ fBodyBodyGyroJerkMagmean: num  -0.942 -0.99 -0.995 -0.319 -0.282 ...
* $ fBodyBodyGyroJerkMagstd : num  -0.933 -0.987 -0.995 -0.382 -0.392 ...


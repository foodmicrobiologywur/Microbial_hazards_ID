
#define default pictogram inputs for all sheets
logo <- div(tags$img(src="https://github.com/alexanderdank/MIDI-app/blob/main/MID_V2.jpg?raw=true", 
                     alt="MI ID logo", 
                     deleteFile=FALSE, 
                     width='200px',height='140px'))

foodmicro <- div(tags$img(src="https://github.com/alexanderdank/MIDI-app/blob/main/Food%20Microbiology.jpg?raw=true", 
                          alt="Food microbiology", 
                          deleteFile=FALSE, 
                          width='140px',height='20px'))




wurlogo <- div(tags$style(".rightAlign{float:right;}"),
               tags$img(src="https://github.com/alexanderdank/MIDI-app/blob/main/WUR_RGB_standard_2021.png?raw=true", 
                        alt="Food microbiology", 
                        deleteFile=FALSE, 
                        width='340px',
                        height='100px',
                        class= 'rightAlign'),
)

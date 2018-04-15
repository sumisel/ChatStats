################################################################################
##### create csvs
  #### export data from whatsapp chat
    ### in the whatsapp chat you want to export, click the dots on the
      ### top right corner
    ### select more -> email chat
    ### select without media
    ### email it to yourself
  #### clean up data in notepad++
    ### open the exported chat
    ### select encoding utf-8
    ### remove first line (notification that chat is encrypted)
    ### replace all '\t' -> ' '
    ### replace names
      ## '- <my name>:' -> 'me:'
      ## '- <your name>:' -> 'you:'
    ### remove some characters with regex '([a-z,A-Z])[\.\,\!\?\-\:]' -> '\1'
      ## repeat until no more matches are found
    ### remove linebreaks within messages
      ## regex '\n([^0-9])' -> ' \1'
        # repeat until no more matches are found
      ## mark all lines that still have linebreaks within messages
        # regex '\n\d\d/'
      ## inverse bookmarks
      ## F2 to find remove linebreaks manually
      ## after you're done, clear all bookmarks
  #### convert to csv
    ### open an empty excel
    ### open the txt within excel
    ### when opening, in the first dialogue select
      ## delimited (as opposed to fixed)
      ## encoding utf-8
    ### on the second page of the dialogue, select
      ## space separated nad nothing else
    ### save as chat.csv
  #### create extra dataset for emojis
    ### open chat in notepad++ again
    ### select encoding utf-8
    ### remove words
      ## regex '([a-z]+)[^0-9]' -> ''
    ### remove dates with regex
      ## '([0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9], [0-9][0-9]:[0-9][0-9])'
      ## -> ''
    ### use html converter plugin to convert emojis
    ### put every emoji in a new line
      ## ' ' -> '\n'
    ### remove non-html with regex '^(?!.*\#)^(?!.*\:).*$' -> ''
    ### more cleanup with regex
      ## ' *[A-Z] *' -> ''
      ## '[0-9]+(&#[0-9]+)' -> '\1'
    ### replace '\n\n' -> '\n'
      ## repeat until no more matches are found
    ### open in excel (without delimiters) and save as 'chat_emojis.csv'
################################################################################

library(ggplot2)
library(qdap)

# read the data
dat <- read.csv('C:\\R\\git\\chat.csv', header = FALSE)
datsenders <- dat[1:3]
colnames(datsenders) <- c('date', 'time', 'sender')
datsenders$sender <- factor(datsenders$sender, levels = c('you', 'me'))
emos <- read.csv('C:\\R\\git\\chat_emojis.csv',
		 header = FALSE,
                 encoding='UTF-8')
words <- apply(dat[ , c(4:ncol(dat)) ] , 1 , paste)

# parse the time
Sys.setlocale('LC_TIME', 'C')
datsenders$iso <- paste(datsenders$date, datsenders$time, sep=' ')
datsenders$iso <- strptime(datsenders$iso, '%d/%m/%Y, %H:%M')
datsenders$month <- factor(months(datsenders$iso, abbreviate=TRUE),
                      # our messages started late March
                      # that's why the plot starts with April
                      levels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
                                 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'))
datsenders$day <- factor(weekdays(datsenders$iso, abbreviate=TRUE),
                         levels = c('Mon', 'Tue', 'Wed', 'Thu',
                                    'Fri', 'Sat', 'Sun'))
datsenders$hour <- format(datsenders$iso, '%H')

# format date column as date datatype
datsenders$date <- as.Date(as.character(datsenders$date), format = '%d/%m/%Y')

# plot formatting
plottheme <- theme(plot.title = element_text(family = 'Helvetica',
                                             colour = 'steelblue',
                                             face = 'bold',
                                             size = (15)), 
                   legend.title = element_text(family = 'Helvetica',
                                               colour = 'steelblue',
                                               face = 'bold.italic'), 
                   legend.text = element_text(family = 'Helvetica',
                                            colour = 'cornflowerblue',
                                            face = 'italic'), 
                   axis.title = element_blank(),
                   axis.text = element_text(family = 'Courier',
                                            colour = 'cornflowerblue',
                                            size = (10)),
                   axis.text.x = element_text(family = 'Courier',
                                              colour = 'cornflowerblue',
                                              size = (10)),
                   axis.text.y = element_text(family = 'Courier',
                                              colour = 'cornflowerblue',
                                              size = (10)))
minimalplottheme <- theme_void() + 
		    theme(plot.title = element_text(family = 'Helvetica',
                                                    colour = 'steelblue',
                                                    face = 'bold',
                                                    size = (15)), 
                          legend.title = element_text(family = 'Helvetica',
                                                      colour = 'steelblue',
                                                      face = 'bold.italic'), 
                          legend.text = element_text(family = 'Helvetica',
                                                     colour = 'cornflowerblue',
                                                     face = 'italic'), 
                          axis.title = element_blank(),
                          axis.text = element_blank())


## stats

# days
tbl <- table(datsenders$sender)
datsenders$date[as.numeric(as.character(tbl['you'])) +
                as.numeric(as.character(tbl['me']))] -
  datsenders$date[1]

# messages
as.numeric(as.character(tbl['you'])) + as.numeric(as.character(tbl['me']))

# words
length(words[words!=''])

# how many texts someone has sent
df <- data.frame(
  sender = factor(c('you', 'me'),
  levels = c('you', 'me')),
  value = c(as.numeric(as.character(tbl['you'])),
            as.numeric(as.character(tbl['me']))))

bp <- 
  ggplot(df, aes(x='', y=value, fill=sender)) +
  geom_bar(width = 1, stat = 'identity')

bp + 
  ggtitle('number of messages sent') + 
  coord_polar('y', start=0) + 
  minimalplottheme + 
  geom_text(aes(y = value/3 + c(0,
                  cumsum(value)[-length(value)]),
                  label = value),
            size=3,
            colour='cornflowerblue') +
  scale_fill_brewer(palette='Pastel2')

# messages over time
ggplot(datsenders,
       aes(x = date, fill = sender)) + 
	 ggtitle('messages over time') + 
       geom_histogram(binwidth = 1) + 
       plottheme + 
       scale_fill_brewer(palette='Pastel2')

# messages by month
ggplot(datsenders,
       aes(x = month, fill = sender)) + 
       ggtitle('messages by month') + 
       geom_histogram(position = 'dodge', stat = 'count') + 
       plottheme +
       scale_fill_brewer(palette='Pastel2')

# messages by weekday
ggplot(datsenders,
       aes(x = day, fill = sender)) + 
       ggtitle('messages by weekday') + 
       geom_histogram(binwidth = 7, position = 'dodge', stat = 'count') + 
       plottheme +
       scale_fill_brewer(palette='Pastel2')

# messages by hour of day
ggplot(datsenders,
       aes(x = hour, fill = sender)) + 
       ggtitle('messages by hour of day') + 
       geom_histogram(position = 'dodge', stat = 'count') + 
       plottheme +
       scale_fill_brewer(palette='Pastel2')

# most frequent words
dataft <- freq_terms(words,
                     10,
                     stopwords = c(qdapDictionaries::Top25Words,
                                   'media',
                                   'omitted')) %>%
	  data.frame()
colnames(dataft) <- c('word', 'freq') # nicer labels in the plot
dataft$word <- factor(dataft$word, levels = dataft$word[order(-dataft$freq)])
ggplot(dataft, aes(word, freq)) +
       ggtitle('most frequent words') +
       geom_col(fill=scale_fill_brewer(palette='Pastel2')$palette(8)[2]) +
       plottheme

# emojis
dataemos <- data.frame(table(emos))
dataemos <- head(dataemos[order(-dataemos$Freq),], 10)
dataemos$emos <- factor(dataemos$emos, levels=head(dataemos$emos,10))

ggplot(dataemos, aes(emos, Freq)) +
       ggtitle('most frequent emojis') +
       geom_col(fill=scale_fill_brewer(palette='Pastel2')$palette(8)[2]) +
       plottheme + theme(axis.text.x = element_blank())

# lastly, make the labels as emojis
dataemos$emos
# copy the output in a browser, e.g. google search
# copy the emoji images from the result
# paste them in the graph with an image manipulation program, e.g. gimp

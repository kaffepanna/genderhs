### Whats done so far.

 * corpus scraping
 * download and unpacking of files
 * generate spectrogram from wav file


# Speech gender classification of spectrogram using CNN in Haskell

## Idea

The idea of this little hobby project is to see if CNNs can be used
to classify speech based of frequency spectrograms.

Intuitively CNNs is probably not the best for this kind of klassification
since a spectrogram cannot be interpreted the same way as a image, since it
has different dimensions alog its axises so a particular (image) feature
does not mean the same thing dependent on where it is. But still im interested
to see how well it might work.

## Implementation

I'll divide the program in two different stages `acquiring data and classification` and
`model creation and verification`

### Acquring data

The basic idea here is to scrap the website for links to the speech corpus (http://www.repository.voxforge1.org/downloads/SpeechCorpus/Trunk/Audio/Main/16kHz_16bit/)
parse out the information (male / female), generate features (spectrogram) and store it in a databases.

### Model creation

Split the dataset in training, verification and testing and building a CNN 
using haskells tensorflow bindings. This will require some experimentation to
find a topology for our CNN.

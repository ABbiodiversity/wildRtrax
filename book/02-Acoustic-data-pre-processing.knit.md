# Acoustic data pre-processing

## Acoustic data management basics

The first step after an ARU is retrieved from the field is to promptly download and secure the data to create a redundant backup. Having redundant backups not only protects against data loss but also ensures business continuity and minimizes downtime in the event of a system failure or natural disaster. Furthermore, in case there is an issue with the SD card copy during the quality control process, having a redundant backup ensures that a copy of the data can be restored.

![](assets/sds.png)

### Acoustic metadata

In `wildRtrax` and WildTrax, there are a few essential components and standards that are required in order for the data to be utilized within this context:

- [**Location**](https://www.wildtrax.ca/home/resources/guide/organizations/locations.html): The physical, geographic place at which environmental sensors were deployed and/or biological data was collected on the landscape. This is how you associate spatial coordinates to the raw audio data (see [Geographic filtering](#geographic-filtering)). 
- **Date and time**: The temporal component of the audio recording. This is when the recording took place. Many time formats are possible but the recommended standard is `YYYYMMDDHHMMSSTZ` wherever possible.
- **Sample rate**: The frequency at which audio waveforms are captured during analog-to-digital conversion in the environment. Sampling rates of 44.1 kHz, 48 kHz, or 96 kHz are commonly used for capturing audio within the 20–20,000 Hz range. Higher sampling rates (>96 kHz) are employed to capture [ultrasonic species](#ultrasonic-data) as needed.

![](assets/task.png)

A **recording** is the raw media or audio file. The following data types are supported within `wildRtrax` package. They are implicitly related to how each model and type of ARU records data. See [Wildlife Acoustics](https://www.wildlifeacoustics.com/), [Frontier Labs](https://www.frontierlabs.com.au/bar-lt) and [Open Acoustic Devices](https://www.openacousticdevices.info/audiomoth) for examples to learn more. 

Audio files need to contain both spatial and temporal information which is the minimum required in order to upload media to WildTrax, e.g. `ABMI-538-SW_20220506_050000`, where `ABMI-538-SW` is the location and `20220506_050000` is the timestamp. Both these pieces of information can tell you when and where the recording took place.

:::: {.dangerbox data-latex=""}
::: {.left data-latex=""}
*Keep in mind*<br>
If the timestamp does not contain a timezone, you can add one if your location has spatial coordinates using `wt_audio_scanner`.
:::
::::


```r
location_name <- "ABMI-1025-SW"
timestamp <- "20220406_074500"
file_type <- ".wav"

# A standard file name from the ABMI
paste0(location_name, "_", timestamp, file_type)
```

```
## [1] "ABMI-1025-SW_20220406_074500.wav"
```

Some additional file name recommendations include:

* Omitting leading zeros for numeric delimited content, e.g. use `OG-1-3-5` instead of `OG-01-003-05`, unless they serve a identification purpose, e.g. `3-0-A12` where `-0-` indicates the an absence of a treatment 
* Delimiters such as “-”, “_”, “@” are supported
* Location name strings that appear numeric (e.g. 35001) should be treated with caution when importing via csv
* Spaces and slashes “/” “\” in location names are not supported 
* “_000” suffixes from Wildlife Acoustics Kaleidoscope output (wac -> wav conversion) are supported

### Acoustic file formats

An audio file format is a digital file format used to store and transmit audio data. It defines how the audio data is organized, compressed, and encoded. Audio file formats can be categorized as uncompressed, lossless compressed, and lossy compressed formats.

Uncompressed formats, such as WAV store audio data without any compression, resulting in high-quality audio but larger file sizes. Lossless compressed formats, such as FLAC, compress the audio data without losing any information, resulting in smaller file sizes without any loss in quality. Lossy compressed formats, such as MP3, use data compression algorithms to reduce the file size by discarding some of the audio data, resulting in a lower-quality audio file.

Different audio file formats have different advantages and disadvantages, and their use depends on the intended application and specific requirements. `wildRtrax` strongly recommends recording raw audio data either in following formats:
 
* **wac** are proprietary, lossless compressed file formats developed by Wildlife Acoustics
* **wav** is the standard, ubiquitous uncompressed audio file format
* **flac** is a lossless compressed audio file format

:::: {.didyouknow data-latex=""}
::: {.left data-latex=""}
*Did you know?*<br>
When data are uploaded to WildTrax, the audio is converted and stored as flac.
:::
::::

**mp3** is a lossy compressed audio file format and works by reducing the accuracy of certain sound components, and eliminating others. It is not typically recommended for raw audio recordings (see [MacPhail et al. 2023]()). 

## Manipulating recordings

Monitoring programs often generate an enormous amount of audio data that exceeds human capacity to listen to. To overcome this challenge, we can select specific audio files for upload to WildTrax and processing using predetermined criteria. This approach not only prioritizes important data but also helps minimize data storage costs while maximizing processing power for calculating relevant biological metrics.

:::: {.dangerbox data-latex=""}
::: {.left data-latex=""}
*Keep in mind*<br>
If you're not an R user you can stop here and upload all of your recordings directly an organization on WildTrax. WildTrax however does not contain all the necessary data quality control measures to uptake data that are included in `wildRtrax`.

- Go to the organization on WildTrax
- Go to Manage > Upload Recordings to Organization
- Follow the steps in WildTrax to finalize data upload
:::
::::

### Reading files

The first step in data pre-processing is to determine what files exist. `wt_audio_scanner` is the function that provides the basis for the rest of the data pre-processing steps. The function recursively scans directories of audio data and returns standard metadata such as the file path, file name, file size, date, time, location name, that will later be used in WildTrax.

```r
files <- wt_audio_scanner(path = "../ABMI/ARU/ABMI-EH/2022/V1/228/228-NE", file_type = "wav", extra_cols = F)
```
























































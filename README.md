# BirdNET Postprocessing
Process raw audio files using BirdNET to generate species detections, embeddings, and clusters by call type, as well as representative training samples for validation. 

# Suggested Pipeline:

Run audio file splitter / renamer
Generate BirdNET detections
Aggregate BirdNET detections
Generate BirdNET embeddings
Aggregatae BirdNET embeddings
Cluster over BirdNET embeddings by species to call type
Sample representative call segments
- 1-minute segments
- 3-second segments
- stratify by call type, confidence quantile (0-50%, 50-75%, 75-87.5%, 87.5%-100%)
Export samples to Whombat

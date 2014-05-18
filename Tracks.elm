module Tracks where
import Dict
import Set
import Graphics.Input

andreasFavorites = Set.fromList [
    "DD2458 Problem Solving and Programming under Pressure",
    "DD2387 Program System Construction Using C++",
    "DD2438 Artificial Intelligence and Multi Agent Systems",
    "DD2448 Foundations of Cryptography",
    "DD2488 Compiler Construction",
    "DD2431 Machine Learning",
    "EN2202 Pattern Recognition",
    "SF1811 Optimization",
    "EP2500 Networked Systems Security"
  ]

checkboxes = map (\_->Graphics.Input.input False) (Set.toList courses)

input : Signal [Bool]
input = combine <| map .signal checkboxes

main = layout <~ input
layout checks = flow right [chooseCourses checks, bestTracks checks]--, missingFromBest]


chooseCourses checks = let
  row (check, (checked, name)) = flow right [
      Graphics.Input.checkbox check.handle id checked,
      plainText name
    ]
  triple = zip checkboxes <| zip checks <| Set.toList courses
    in flow down (map row triple)

bestTracks checks = let
  test ss =
    length <|
      Set.toList <|
        Set.filter (\x -> Set.member x (favorites checks)) ss
  line (name, n) = flow right [
      asText n,
      plainText name
    ]
    in
  flow down <|
    map line <|
      reverse . sortBy snd <|
        Dict.toList <|
          Dict.map test tracks

courses : Set.Set String
courses = foldl Set.union Set.empty (Dict.values tracks)

favorites : [Bool] -> Set.Set String
favorites checks = Set.fromList <| map snd <| filter fst <| zip checks <| Set.toList courses

tracks = Dict.fromList [
  ("Autonomous Systems (CSCA)", Set.fromList [
    "DD2431 Machine Learning",
    "EN2202 Pattern Recognition",
    "ID2213 Logic Programming",
    "DD2423 Image Analysis and Computer Vision",
    "ID2209 Distributed Artificial Intelligence and Intelligent Agents",
    "SF1811 Optimization",
    "DD2432 Artificial Neural Networks and Other Learning Systems",
    "DT2112 Speech Technology",
    "DD2438 Artificial Intelligence and Multi Agent Systems  60023",
    "DD2476 Search Engines and Information Retrieval Systems",
    "DD2427 Image Based Recognition and Classification",
    "EL3320 Applied Estimation",
    "EQ1240 Signal Processing"
  ]),
  ("Computer Security (CSCB)", Set.fromList [
    "DD2387 Program System Construction Using C++",
    "DH2620 Human-Computer Interaction, Introductory Course",
    "EP2500 Networked Systems Security",
    "EP2510 Advanced Networked Systems Security",
    "DD2459 Software Reliability",
    "DD2448 Foundations of Cryptography",
    "DD2458 Problem Solving and Programming under Pressure",
    "EP2520 Building Networked Systems Security"
  ]),
  ("IT-Management with Enterprise Architecture (CSCC)", Set.fromList [
    "EH2010 Management of Technology",
    "DH2620 Human-Computer Interaction, Introductory Course",
    "Only for them who has not been reading DH1620.",
    "EH2781 IT Management with Enterprise Architecture II, Case Studies  50936",
    "ME2042 Business Negotiations",
    "EH2030 Business Development and Quality Management",
    "AK2014 Decision Theory",
    "DD2459 Software Reliability",
    "ME1003 Industrial Management, Basic Course",
    "DD2471 Modern Database Systems and Their Applications",
    "EH2770 IT Management with Enterprise Architecture I",
    "EH2790 Requirements Engineering, Introductory Course",
    "EP2520 Building Networked Systems Security"
  ]),
  ("Program System Technology (CSCD)", Set.fromList [
    "DD2431 Machine Learning",
    "DD2387 Program System Construction Using C++",
    "DH2620 Human-Computer Interaction, Introductory Course",
    "DD2386 Patterns for Large-scale Development",
    "DD2418 Language Engineering",
    "DD2390 Internet Programming",
    "ID1217 Concurrent Programming",
    "DD2388 Program System Construction using .NET Framework",
    "DD2458 Problem Solving and Programming under Pressure",
    "DD2476 Search Engines and Information Retrieval Systems",
    "DD2488 Compiler Construction",
    "DD2457 Program Semantics and Analysis",
    "DD2471 Modern Database Systems and Their Applications"
  ]),
  ("Language Technology (CSCE)", Set.fromList [
    "DD2431 Machine Learning",
    "DD2387 Program System Construction Using C++",
    "DH2620 Human-Computer Interaction, Introductory Course",
    "DD2418 Language Engineering",
    "DT2140 Multimodal Interaction and Interfaces",
    "DD2390 Internet Programming",
    "DT2112 Speech Technology",
    "DD2476 Search Engines and Information Retrieval Systems",
    "DD2457 Program Semantics and Analysis",
    "SF1904 Markov Processes, Basic Course"
  ]),
  ("Theoretical Computer Science (CSCF)", Set.fromList [
    "EQ1220 Signal Theory",
    "DD2447 Statistical Methods in Applied Computer Science",
    "SF1811 Optimization",
    "SF2729 Groups and Rings",
    "ID1217 Concurrent Programming",
    "DD2448 Foundations of Cryptography",
    "DD2458 Problem Solving and Programming under Pressure",
    "DD2457 Program Semantics and Analysis",
    "DD2441 Seminars on Theoretical Computer Science"
  ]),
  ("Computational Biology (CSCG)", Set.fromList [
    "BB2440 Bioinformatics and Biostatistics",
    "BB2510 Proteomics",
    "DD2431 Machine Learning",
    "EL1820 Modelling of Dynamical Systems",
    "EN2202 Pattern Recognition",
    "SF2940 Probability Theory",
    "SK2530 Introduction to Biomedicine",
    "BB2470 Genetics and Genomics  50157",
    "DD2402 Advanced Individual Course in Computational Biology",
    "DD2435 Mathematical Modelling of Biological Systems",
    "SK2520 Experimental Methods in Molecular Biophysics",
    "DD2404 Applied Bioinformatics",
    "DD2447 Statistical Methods in Applied Computer Science",
    "EL2620 Nonlinear Control",
    "SF1811 Optimization",
    "DD2390 Internet Programming",
    "DD2399 Omic Data and Systems Biology",
    "DD2432 Artificial Neural Networks and Other Learning Systems",
    "SF2950 Applied Mathematical Statistics",
    "DD2400 Cellular and Molecular Biology   60036",
    "DD2476 Search Engines and Information Retrieval Systems",
    "DD2257 Visualization",
    "DD2398 Quantitative Systems Biology",
    "DD2401 Neuroscience",
    "SF2943 Time Series Analysis",
    "BB2250 Applied Gene Technology",
    "EN2500 Information Theory and Source Coding"
  ]),
  ("Computer Systems Engineering (CSCH)", Set.fromList [
    "IL2206 Embedded Systems",
    "IL2217 Digital Design with HDL",
    "ID2202 Compilers and Execution Environments",
    "Either DD2488 or ID2202 can be taken",
    "IS2200 Parallel Computer Systems",
    "ID1217 Concurrent Programming",
    "IL2212 Embedded Software",
    "DD2488 Compiler Construction",
    "IS2202 Computer Systems Architecture",
    "IS2205 Individual Studies in Computer Systems"
  ]),
  ("Sound and Music Computing (CSCI)", Set.fromList [
    "DT2300 Sound in Interaction",
    "DT2350 Human Perception for Information Technology",
    "EN2202 Pattern Recognition",
    "DT2215 Advanced Individual Course in Music Communication",
    "DT1130 Spectral Transforms",
    "DT2140 Multimodal Interaction and Interfaces",
    "DT2410 Audio Technology",
    "DT2112 Speech Technology",
    "DT2212 Music Acoustics",
    "DT2118 Speech and Speaker Recognition",
    "DT2213 Musical Communication and Music Technology"
  ])
  ]
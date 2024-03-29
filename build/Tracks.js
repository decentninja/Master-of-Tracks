Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Main";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Dict = Elm.Dict.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Input = Elm.Graphics.Input.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Set = Elm.Set.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var tracks = Dict.fromList(_L.fromArray([{ctor: "_Tuple2"
                                            ,_0: "Autonomous Systems (CSCA)"
                                            ,_1: Set.fromList(_L.fromArray(["DD2431 Machine Learning"
                                                                           ,"EN2202 Pattern Recognition"
                                                                           ,"ID2213 Logic Programming"
                                                                           ,"DD2423 Image Analysis and Computer Vision"
                                                                           ,"ID2209 Distributed Artificial Intelligence and Intelligent Agents"
                                                                           ,"SF1811 Optimization"
                                                                           ,"DD2432 Artificial Neural Networks and Other Learning Systems"
                                                                           ,"DT2112 Speech Technology"
                                                                           ,"DD2438 Artificial Intelligence and Multi Agent Systems  60023"
                                                                           ,"DD2476 Search Engines and Information Retrieval Systems"
                                                                           ,"DD2427 Image Based Recognition and Classification"
                                                                           ,"EL3320 Applied Estimation"
                                                                           ,"EQ1240 Signal Processing"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Computer Security (CSCB)"
                                            ,_1: Set.fromList(_L.fromArray(["DD2387 Program System Construction Using C++"
                                                                           ,"DH2620 Human-Computer Interaction, Introductory Course"
                                                                           ,"EP2500 Networked Systems Security"
                                                                           ,"EP2510 Advanced Networked Systems Security"
                                                                           ,"DD2459 Software Reliability"
                                                                           ,"DD2448 Foundations of Cryptography"
                                                                           ,"DD2458 Problem Solving and Programming under Pressure"
                                                                           ,"EP2520 Building Networked Systems Security"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "IT-Management with Enterprise Architecture (CSCC)"
                                            ,_1: Set.fromList(_L.fromArray(["EH2010 Management of Technology"
                                                                           ,"DH2620 Human-Computer Interaction, Introductory Course"
                                                                           ,"Only for them who has not been reading DH1620."
                                                                           ,"EH2781 IT Management with Enterprise Architecture II, Case Studies  50936"
                                                                           ,"ME2042 Business Negotiations"
                                                                           ,"EH2030 Business Development and Quality Management"
                                                                           ,"AK2014 Decision Theory"
                                                                           ,"DD2459 Software Reliability"
                                                                           ,"ME1003 Industrial Management, Basic Course"
                                                                           ,"DD2471 Modern Database Systems and Their Applications"
                                                                           ,"EH2770 IT Management with Enterprise Architecture I"
                                                                           ,"EH2790 Requirements Engineering, Introductory Course"
                                                                           ,"EP2520 Building Networked Systems Security"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Program System Technology (CSCD)"
                                            ,_1: Set.fromList(_L.fromArray(["DD2431 Machine Learning"
                                                                           ,"DD2387 Program System Construction Using C++"
                                                                           ,"DH2620 Human-Computer Interaction, Introductory Course"
                                                                           ,"DD2386 Patterns for Large-scale Development"
                                                                           ,"DD2418 Language Engineering"
                                                                           ,"DD2390 Internet Programming"
                                                                           ,"ID1217 Concurrent Programming"
                                                                           ,"DD2388 Program System Construction using .NET Framework"
                                                                           ,"DD2458 Problem Solving and Programming under Pressure"
                                                                           ,"DD2476 Search Engines and Information Retrieval Systems"
                                                                           ,"DD2488 Compiler Construction"
                                                                           ,"DD2457 Program Semantics and Analysis"
                                                                           ,"DD2471 Modern Database Systems and Their Applications"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Language Technology (CSCE)"
                                            ,_1: Set.fromList(_L.fromArray(["DD2431 Machine Learning"
                                                                           ,"DD2387 Program System Construction Using C++"
                                                                           ,"DH2620 Human-Computer Interaction, Introductory Course"
                                                                           ,"DD2418 Language Engineering"
                                                                           ,"DT2140 Multimodal Interaction and Interfaces"
                                                                           ,"DD2390 Internet Programming"
                                                                           ,"DT2112 Speech Technology"
                                                                           ,"DD2476 Search Engines and Information Retrieval Systems"
                                                                           ,"DD2457 Program Semantics and Analysis"
                                                                           ,"SF1904 Markov Processes, Basic Course"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Theoretical Computer Science (CSCF)"
                                            ,_1: Set.fromList(_L.fromArray(["EQ1220 Signal Theory"
                                                                           ,"DD2447 Statistical Methods in Applied Computer Science"
                                                                           ,"SF1811 Optimization"
                                                                           ,"SF2729 Groups and Rings"
                                                                           ,"ID1217 Concurrent Programming"
                                                                           ,"DD2448 Foundations of Cryptography"
                                                                           ,"DD2458 Problem Solving and Programming under Pressure"
                                                                           ,"DD2457 Program Semantics and Analysis"
                                                                           ,"DD2441 Seminars on Theoretical Computer Science"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Computational Biology (CSCG)"
                                            ,_1: Set.fromList(_L.fromArray(["BB2440 Bioinformatics and Biostatistics"
                                                                           ,"BB2510 Proteomics"
                                                                           ,"DD2431 Machine Learning"
                                                                           ,"EL1820 Modelling of Dynamical Systems"
                                                                           ,"EN2202 Pattern Recognition"
                                                                           ,"SF2940 Probability Theory"
                                                                           ,"SK2530 Introduction to Biomedicine"
                                                                           ,"BB2470 Genetics and Genomics  50157"
                                                                           ,"DD2402 Advanced Individual Course in Computational Biology"
                                                                           ,"DD2435 Mathematical Modelling of Biological Systems"
                                                                           ,"SK2520 Experimental Methods in Molecular Biophysics"
                                                                           ,"DD2404 Applied Bioinformatics"
                                                                           ,"DD2447 Statistical Methods in Applied Computer Science"
                                                                           ,"EL2620 Nonlinear Control"
                                                                           ,"SF1811 Optimization"
                                                                           ,"DD2390 Internet Programming"
                                                                           ,"DD2399 Omic Data and Systems Biology"
                                                                           ,"DD2432 Artificial Neural Networks and Other Learning Systems"
                                                                           ,"SF2950 Applied Mathematical Statistics"
                                                                           ,"DD2400 Cellular and Molecular Biology   60036"
                                                                           ,"DD2476 Search Engines and Information Retrieval Systems"
                                                                           ,"DD2257 Visualization"
                                                                           ,"DD2398 Quantitative Systems Biology"
                                                                           ,"DD2401 Neuroscience"
                                                                           ,"SF2943 Time Series Analysis"
                                                                           ,"BB2250 Applied Gene Technology"
                                                                           ,"EN2500 Information Theory and Source Coding"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Computer Systems Engineering (CSCH)"
                                            ,_1: Set.fromList(_L.fromArray(["IL2206 Embedded Systems"
                                                                           ,"IL2217 Digital Design with HDL"
                                                                           ,"ID2202 Compilers and Execution Environments"
                                                                           ,"Either DD2488 or ID2202 can be taken"
                                                                           ,"IS2200 Parallel Computer Systems"
                                                                           ,"ID1217 Concurrent Programming"
                                                                           ,"IL2212 Embedded Software"
                                                                           ,"DD2488 Compiler Construction"
                                                                           ,"IS2202 Computer Systems Architecture"
                                                                           ,"IS2205 Individual Studies in Computer Systems"]))}
                                           ,{ctor: "_Tuple2"
                                            ,_0: "Sound and Music Computing (CSCI)"
                                            ,_1: Set.fromList(_L.fromArray(["DT2300 Sound in Interaction"
                                                                           ,"DT2350 Human Perception for Information Technology"
                                                                           ,"EN2202 Pattern Recognition"
                                                                           ,"DT2215 Advanced Individual Course in Music Communication"
                                                                           ,"DT1130 Spectral Transforms"
                                                                           ,"DT2140 Multimodal Interaction and Interfaces"
                                                                           ,"DT2410 Audio Technology"
                                                                           ,"DT2112 Speech Technology"
                                                                           ,"DT2212 Music Acoustics"
                                                                           ,"DT2118 Speech and Speaker Recognition"
                                                                           ,"DT2213 Musical Communication and Music Technology"]))}]));
   var unique = function (a) {
      return Set.toList(Set.fromList(a));
   };
   var courses = unique(List.concat(A2(List.map,
   Set.toList,
   Dict.values(tracks))));
   var favorites = function (checks) {
      return Set.fromList(List.map(Basics.snd)(List.filter(Basics.fst)(List.zip(checks)(courses))));
   };
   var bestTracks = function (checks) {
      return function () {
         var order = function ($) {
            return List.reverse(List.sortBy(Basics.snd)($));
         };
         var line = function (_v0) {
            return function () {
               switch (_v0.ctor)
               {case "_Tuple2":
                  return A2(Graphics.Element.flow,
                    Graphics.Element.right,
                    _L.fromArray([Text.asText(_v0._1)
                                 ,Text.plainText(_v0._0)]));}
               _E.Case($moduleName,
               "on line 34, column 20 to 57");
            }();
         };
         var filt = function (x) {
            return A2(Set.member,
            x,
            favorites(checks));
         };
         var amount = function (ss) {
            return List.length(Set.toList(A2(Set.filter,
            filt,
            ss)));
         };
         return Graphics.Element.flow(Graphics.Element.down)(List.map(line)(order(Dict.toList(A2(Dict.map,
         amount,
         tracks)))));
      }();
   };
   var checkboxes = A2(List.map,
   function (_v4) {
      return function () {
         return Graphics.Input.input(false);
      }();
   },
   courses);
   var chooseCourses = function (checks) {
      return function () {
         var triple = List.zip(checkboxes)(List.zip(checks)(courses));
         var row = function (_v6) {
            return function () {
               switch (_v6.ctor)
               {case "_Tuple2":
                  switch (_v6._1.ctor)
                    {case "_Tuple2":
                       return A2(Graphics.Element.flow,
                         Graphics.Element.right,
                         _L.fromArray([A3(Graphics.Input.checkbox,
                                      _v6._0.handle,
                                      Basics.id,
                                      _v6._1._0)
                                      ,Text.plainText(_v6._1._1)]));}
                    break;}
               _E.Case($moduleName,
               "between lines 22 and 25");
            }();
         };
         return A2(Graphics.Element.flow,
         Graphics.Element.down,
         A2(List.map,row,triple));
      }();
   };
   var layout = function (checks) {
      return A2(Graphics.Element.flow,
      Graphics.Element.right,
      _L.fromArray([chooseCourses(checks)
                   ,bestTracks(checks)]));
   };
   var input = Signal.combine(A2(List.map,
   function (_) {
      return _.signal;
   },
   checkboxes));
   var main = A2(Signal._op["<~"],
   layout,
   input);
   _elm.Main.values = {_op: _op
                      ,main: main
                      ,input: input
                      ,checkboxes: checkboxes
                      ,layout: layout
                      ,chooseCourses: chooseCourses
                      ,bestTracks: bestTracks
                      ,favorites: favorites
                      ,unique: unique
                      ,courses: courses
                      ,tracks: tracks};
   return _elm.Main.values;
};
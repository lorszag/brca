filter_avengers_ex15 = function(data, start, alt){
  data%>%
    filter(start == "32356426"|
             (start == "32356427" & alt == "C")|
             (start == "32356429" & alt == "A")|
             (start == "32356431" & alt == "G")| 
             (start == "32356442" & alt == "T")|
             (start == "32356445" & alt == "T")|
             (start == "32356446" & (alt == "T"| alt == "C"))|
             (start == "32356451" & alt == "T")|
             (start == "32356454" & alt == "T")|
             (start == "32356456" & alt == "T")|
             (start == "32356463" & alt == "T")|
             (start == "32356467" & alt == "G")|
             (start == "32356467" & alt == "G")|
             (start == "32356473" & (alt == "T"| alt == "C"))|
             (start == "32356476" & alt == "A")|
             (start == "32356478" & alt == "T")|
             (start == "32356481" & alt == "T")|
             (start == "32356484" & alt == "T")|
             (start == "32356487" & alt == "T")|
             (start == "32356490" & alt == "T")|
             (start == "32356493" & alt == "T")|
             (start == "32356508" & alt == "T")|
             (start == "32356514" & alt == "C")|
             (start == "32356515" & (alt == "T"| alt == "A"))|
             (start == "32356521" & alt == "C")|
             (start == "32356523" & alt == "C")|
             (start == "32356525" & (alt == "G"| alt == "A"))|
             (start == "32356527" & alt == "G")|
             (start == "32356532" & alt == "T")|
             (start == "32356533" & alt == "C")|
             (start == "32356534" & alt == "C")|
             (start == "32356536" & alt == "G")|
             (start == "32356539" & (alt == "G"| alt == "A"))|
             (start == "32356550" & (alt == "T"| alt == "G"))|
             (start == "32356551" & alt == "C")|
             (start == "32356553" & alt == "T")|
             (start == "32356554" & alt == "G")|
             (start == "32356555" & alt == "G")|
             (start == "32356556" & alt == "A")|
             (start == "32356562" & alt == "T")|
             (start == "32356563" & alt == "T")|
             (start == "32356568" & alt == "A")|
             (start == "32356569" & alt == "A")|
             (start == "32356571" & alt == "A")|
             (start == "32356572" & alt == "G")|
             (start == "32356574" & alt == "T")|
             (start == "32356580" & alt == "T")|
             (start == "32356597" & alt == "A")|
             (start == "32356604" & alt == "T")|
             (start == "32356607" & alt == "T")|
             (start == "32356610")|
             (start == "32356611")
    )
}

filter_avengers_ex16 = function(data, start, alt){
  data%>%
    filter(start == "32357740"|
             (start == "32357741" & alt %in% c("T, C"))|
             (start == "32357746" & alt == "T")|
             (start == "32357753" & alt %in% c("G, A"))|
             (start == "32357758" & alt == "G")|
             (start == "32357763" & alt == "T")|
             (start == "32357769" & alt == "A")|
             (start == "32357771" & alt == "A")|
             (start == "32357775" & alt == "T")|
             (start == "32357784" & alt == "T")|
             (start == "32357785" & alt == "A")|
             (start == "32357786" & alt == "G")|
             (start == "32357791" & alt == "T")|
             (start == "32357794")|
             (start == "32357804" & alt == "A")|
             (start == "32357806" & alt == "C")|
             (start == "32357807" & alt == "T")|
             (start == "32357808" & alt %in% c("C, A"))|
             (start == "32357809" & alt == "G")|
             (start == "32357810" & alt %in% c("G, A"))|
             (start == "32357821" & alt == "C")|
             (start == "32357823" & alt == "G")|
             (start == "32357825" & alt == "A")|
             (start == "32357826" & alt == "C")|
             (start == "32357827")|
             (start == "32357832" & alt == "T")|
             (start == "32357837" & alt == "T")|
             (start == "32357842" & alt %in% c("G, A"))|
             (start == "32357845" & alt == "A")|
             (start == "32357846" & alt == "A")|
             (start == "32357850" & alt == "T")|
             (start == "32357853" & alt == "T")|
             (start == "32357856" & alt == "T")|
             (start == "32357862" & alt == "T")|
             (start == "32357866" & alt %in% c("G, A"))|
             (start == "32357871" & alt %in% c("T, C"))|
             (start == "32357872" & alt == "T")|
             (start == "32357874" & alt == "T")|
             (start == "32357877")|
             (start == "32357878" & alt == "T")|
             (start == "32357881" & alt == "A")|
             (start == "32357882" & alt == "A")|
             (start == "32357884")|
             (start == "32357887" & alt == "G")|
             (start == "32357890" & alt == "T")|
             (start == "32357901" & alt == "T")|
             (start == "32357902" & alt == "A")|
             (start == "32357904" & alt == "T")|
             (start == "32357907" & alt == "C")|
             (start == "32357908" & alt == "A")|
             (start == "32357910")|
             (start == "32357911" & alt %in% c("T","A"))|
             (start == "32357913" & alt %in% c("T","G"))|
             (start == "32357914" & alt == "T")|
             (start == "32357915" & alt == "T")|
             (start == "32357916" & alt == "T")|
             (start == "32357917" & alt == "T")|
             (start == "32357919" & alt %in% c("T","A"))|
             (start == "32357920" & alt == "G")|
             (start == "32357922" & alt == "G")|
             (start == "32357923" & alt == "C")|
             (start == "32357925" & alt %in% c("G","A"))|
             (start == "32357926" & alt %in% c("G","C"))|
             (start == "32357927" & alt == "A")|
             (start %in% c("32357929", "32357930", "32357931"))
    )
}

filter_avengers_ex17 = function(data, start, alt){
  data%>%
    filter(start == '32362521' |
             start == '32362522' |
             (start == '32362523' & alt == "T")|
             (start == '32362524' & alt == "C")|
             (start == '32362528')|
             (start == '32362530' & alt == "G")|
             (start == '32362532' & alt %in% c("G", "A"))|
             (start == '32362536' & alt == "C")|
             (start == '32362540' & alt == "T")|
             (start == '32362542' & alt == "C")|
             (start == '32362543' & alt == "T")|
             (start == '32362546' & alt %in% c("G", "A"))|
             (start == '32362549' & alt == "G")|
             (start == '32362554' & alt == "T")|
             (start == '32362558' & alt == "C")|
             (start == '32362561' & alt %in% c("G", "A"))|
             (start == '32362566' & alt == "T")|
             (start == '32362572')|
             (start == '32362573')|
             (start == '32362574')|
             (start == '32362576' & alt == "A")|
             (start == '32362580' & alt %in% c("G", "A"))|
             (start == '32362581' & alt %in% c("T", "G"))|
             (start == '32362582' & alt %in% c("T", "C"))|
             (start == '32362583' & alt == "G")|
             (start == '32362584' & alt %in% c("T", "G"))|
             (start == '32362585')|
             (start == '32362587')|
             (start == '32362588' & alt == "C")|
             (start == '32362589' & alt %in% c("G", "A"))|
             (start == '32362590' & alt %in% c("T", "G"))|
             (start == '32362591' & alt %in% c("T", "C"))|
             (start == '32362592' & alt %in% c("T", "C"))|
             (start == '32362593')|
             (start == '32362594' & alt %in% c("C", "A"))|
             (start == '32362595')|
             (start == '32362596' & alt == "T")|
             (start == '32362597' & alt == "A")|
             (start == '32362598' & alt == "G")|
             (start == '32362600' & alt %in% c("G", "A"))|
             (start == '32362602' & alt == "G")|
             (start == '32362603' & alt == "A")|
             (start == '32362604' & alt %in% c("T", "A"))|
             (start == '32362605')|
             (start == '32362606' & alt %in% c("T", "C"))|
             (start == '32362607' & alt %in% c("T", "C"))|
             (start == '32362609' & alt %in% c("G", "C"))|
             (start == '32362611' & alt == "C")|
             (start == '32362612' & alt == "A")|
             (start == '32362614' & alt == "C")|
             (start == '32362615' & alt == "A")|
             (start == '32362618' & alt == "G")|
             (start == '32362620' & alt %in% c("T", "A"))|
             (start == '32362621' & alt == "G")|
             (start == '32362625' & alt == "A")|
             (start == '32362626' & alt == "C")|
             (start == '32362635' & alt == "T")|
             (start == '32362638' & alt == "T")|
             (start == '32362641' & alt %in% c("G", "A"))|
             (start == '32362642' & alt == "C")|
             (start == '32362650' & alt == "T")|
             (start == '32362655' & alt == "A")|
             (start == '32362657')|
             (start == '32362662' & alt == "T")|
             (start == '32362663' & alt %in% c("T", "G"))|
             (start == '32362665' & alt == "T")|
             (start == '32362668' & alt == "G")|
             (start == '32362669')|
             (start == '32362670' & alt == "T")|
             (start == '32362671' & alt %in% c("T", "A"))|
             (start == '32362672' & alt == "G")|
             (start == '32362674' & alt == "G")|
             (start == '32362675' & alt == "C")|
             (start == '32362678' & alt == "C")|
             (start == '32362680')|
             (start == '32362683' & alt == "G")|
             (start == '32362684')|
             (start == '32362686' & alt %in% c("T", "G"))|
             (start == '32362686' & alt %in% c("T", "C"))|
             (start == '32362688' & alt == "T")|
             (start == '32362691' & alt %in% c("G", "A"))|
             (start == '32362692' & alt == "T")|
             (start == '32362693')|
             (start == '32362695'))
}

filter_avengers_ex18 = function(data, start, alt){
  data%>%filter(
    (start == '32363177' |
       start == '32363178' |
       (start == '32363179' & alt == "G")|
       start == '32363180' |
       (start == '32363182' & alt %in% c("G", "A"))|
       (start == '32363183' & alt == "T")|
       (start == '32363184' & alt %in% c("T", "C"))|
       (start == '32363189' & alt %in% c("T", "A"))|
       (start == '32363190')|
       (start == '32363191' & alt %in% c("T", "C"))|
       (start == '32363198' & alt == "T")|
       (start == '32363204' & alt == "T")|
       (start == '32363207' & alt == "T")|
       (start == '32363211' & alt %in% c("T", "A"))|
       (start == '32363214' & alt %in% c("G", "A"))|
       (start == '32363217')|
       (start == '32363219' & alt == "T")|
       (start == '32363222' & alt == "T")|
       (start == '32363225' & alt == "G")|
       (start == '32363226' & alt %in% c("G", "A"))|
       (start == '32363229' & alt == "G")|
       (start == '32363231' & alt == "T")|
       (start == '32363232' & alt == "T")|
       (start == '32363234' & alt == "T")|
       (start == '32363252' & alt == "T")|
       (start == '32363259')|
       (start == '32363261' & alt == "T")|
       (start == '32363262' & alt %in% c("G", "A"))|
       (start == '32363264' & alt == "A")|
       (start == '32363265')|
       (start == '32363269' & alt %in% c("G", "A"))|
       (start == '32363271' & alt %in% c("G", "A"))|
       (start == '32363273' & alt == "C")|
       (start == '32363274' & alt %in% c("T", "A"))|
       (start == '32363277' & alt == "T")|
       (start == '32363286' & alt %in% c("G", "A"))|
       (start == '32363289' & alt == "A")|
       (start == '32363306' & alt == "T")|
       (start == '32363321' & alt == "T")|
       (start == '32363342' & alt == "T")|
       (start == '32363345' & alt == "T")|
       (start == '32363352' & alt == "A")|
       (start == '32363357' & alt == "T")|
       (start == '32363358' & alt %in% c("G", "A"))|
       (start == '32363360' & alt == "T")|
       (start == '32363361' & alt %in% c("T", "G"))|
       (start == '32363362' & alt %in% c("T", "C"))|
       (start == '32363364')|
       (start == '32363366' & alt == "G")|
       (start == '32363367' & alt == "T")|
       (start == '32363369' & alt %in% c("C", "A"))|
       (start == '32363370' & alt == "T")|
       (start == '32363371' & alt == "A")|
       (start == '32363372')|
       (start == '32363373' & alt %in% c("T", "A"))|
       (start == '32363375')|
       (start == '32363376' & alt %in% c("C", "A"))|
       (start == '32363377')|
       (start == '32363378' & alt %in% c("G", "A"))|
       (start == '32363379' & alt == "G")|
       (start == '32363380' & alt %in% c("G", "A"))|
       (start == '32363385' & alt %in% c("G", "A"))|
       (start == '32363387' & alt %in% c("T", "G"))|
       (start == '32363388' & alt == "T")|
       (start == '32363390' & alt == "C")|
       (start == '32363391' & alt == "A")|
       (start == '32363393' & alt == "T")|
       (start == '32363397' & alt %in% c("G", "A"))|
       (start == '32363399' & alt == "T")|
       (start == '32363400' & alt == "T")|
       (start == '32363408' & alt %in% c("T", "A"))|
       (start == '32363409' & alt %in% c("G", "A"))|
       (start == '32363412' & alt %in% c("G", "A"))|
       (start == '32363421' & alt %in% c("G", "A"))|
       (start == '32363423' & alt == "T")|
       (start == '32363429' & alt == "A")|
       (start == '32363430' & alt %in% c("G", "A"))|
       (start == '32363432' & alt == "T")|
       (start == '32363436' & alt %in% c("G", "A"))|
       (start == '32363444' & alt %in% c("T", "C"))|
       (start == '32363445' & alt %in% c("T", "C"))|
       (start == '32363447' & alt == "T")|
       (start == '32363448' & alt == "C")|
       (start == '32363450')|
       (start == '32363451')|
       (start == '32363452' & alt == "T")|
       (start == '32363454' & alt %in% c("C", "A"))|
       (start == '32363457' & alt %in% c("C", "A"))|
       (start == '32363465' & alt == "T")|
       (start == '32363471' & alt == "T")|
       (start == '32363497' & alt == "A")|
       (start == '32363507' & alt == "T")|
       (start == '32363516' & alt == "T")|
       (start == '32363529')|
       (start == '32363531' & alt %in% c("T", "G"))|
       (start == '32363533' & alt == "C")|
       (start == '32363534')|
       (start == '32363535')))
}

filter_avengers_ex19 = function(data, start, alt){
  data%>%filter(
    (start == '32370400' |
       (start == '32370401' & alt %in% c("C", "A"))|
       (start == '32370403' & alt %in% c("G", "A"))|
       (start == '32370406' & alt == "T")|
       (start == '32370408' & alt == "C")|
       (start == '32370409' & alt == "A")|
       (start == '32370411' & alt == "T")|
       (start == '32370412' & alt == "T")|
       (start == '32370413' & alt %in% c("G", "A"))|
       (start == '32370417' & alt == "C")|
       (start == '32370418' & alt == "A")|
       (start == '32370420' & alt == "G")|
       (start == '32370421' & alt %in% c("C", "A"))|
       (start == '32370426' & alt == "C")|
       (start == '32370427' & alt == "A")|
       (start == '32370432') |
       (start == '32370433') |
       (start == '32370437' & alt %in% c("G", "A"))|
       (start == '32370441' & alt == "T")|
       (start == '32370444' & alt == "T")|
       (start == '32370447') |
       (start == '32370448') |
       (start == '32370465' & alt == "T")|
       (start == '32370469' & alt %in% c("G", "A"))|
       (start == '32370484' & alt %in% c("G", "A"))|
       (start == '32370487' & alt %in% c("G", "A"))|
       (start == '32370490' & alt == "A")|
       (start == '32370504' & alt == "T")|
       (start == '32370507' & alt == "T")|
       (start == '32370521' & alt %in% c("G", "A"))|
       (start == '32370522' & alt == "T")|
       (start == '32370523' & alt == "A")|
       (start == '32370526') |
       (start == '32370527' & alt == "G")|
       (start == '32370529' & alt == "A")|
       (start == '32370532' & alt %in% c("G", "A"))|
       (start == '32370535' & alt %in% c("G", "A"))|
       (start == '32370537' & alt == "T")|
       (start == '32370540' & alt %in% c("T", "G"))|
       (start == '32370541' & alt %in% c("T", "C"))|
       (start == '32370542' & alt %in% c("T", "C"))|
       (start == '32370546' & alt %in% c("G", "A"))|
       (start == '32370547' & alt %in% c("G", "C"))|
       (start == '32370555' & alt == "T")|
       (start == '32370556' & alt %in% c("G", "C"))|
       (start == '32370557' & alt %in% c("T", "C"))|
       (start == '32370558') |
       (start == '32370559')
    ))
}

filter_avengers_ex20 = function(data, start, alt){
  data%>%filter(
    (start == '32370954' |
       (start == '32370955') |
       (start == '32370957' & alt %in% c("T", "A"))|
       (start == '32370958' & alt == "A")|
       (start == '32370962' & alt == "T")|
       (start == '32370963' & alt == "C")|
       (start == '32370965' & alt == "T")|
       (start == '32370968' & alt == "C")|
       (start == '32370972' & alt == "A")|
       (start == '32370977' & alt %in% c("T", "A"))|
       (start == '32370978' & alt == "A")|
       (start == '32370981' & alt %in% c("G", "A"))|
       (start == '32370985' & alt %in% c("G", "A"))|
       (start == '32370992' & alt == "G")|
       (start == '32371001' & alt == "T")|
       (start == '32371002' & alt == "A")|
       (start == '32371008' & alt %in% c("G", "A"))|
       (start == '32371013' & alt == "T")|
       (start == '32371025' & alt == "T")|
       (start == '32371030' & alt %in% c("G", "A"))|
       (start == '32371040' & alt == "T")|
       (start == '32371043' & alt == "T")|
       (start == '32371044' & alt == "G")|
       (start == '32371046' & alt == "T")|
       (start == '32371049' & alt == "T")|
       (start == '32371055' & alt == "C")|
       (start == '32371056' & alt == "G")|
       (start == '32371062' & alt == "C")|
       (start == '32371070' & alt == "T")|
       (start == '32371076' & alt == "T")|
       (start == '32371082' & alt == "T")|
       (start == '32371088' & alt == "T")|
       (start == '32371091' & alt == "T")|
       (start == '32371097' & alt == "T")|
       (start == '32371098' & alt == "T")|
       (start == '32371100' & alt %in% c("T", "C"))|
       (start == '32371101') |
       (start == '32371102') 
    ))
}

filter_avengers_ex21 = function(data, start, alt){
  data%>%filter(
    (start == '32376668'|
       (start == '32376669')|
       (start == '32376681' & alt == "T")|
       (start == '32376689' & alt %in% c("G", "A"))|
       (start == '32376691' & alt %in% c("G", "A"))|
       (start == '32376711' & alt == "T")|
       (start == '32376714' & alt == "T")|
       (start == '32376717' & alt == "T")|
       (start == '32376730' & alt == "A")|
       (start == '32376732' & alt == "T")|
       (start == '32376733' & alt == "C")|
       (start == '32376744' & alt == "T")|
       (start == '32376752' & alt == "A")|
       (start == '32376753' & alt == "T")|
       (start == '32376762' & alt == "T")|
       (start == '32376785' & alt %in% c("G", "A"))|
       (start == '32376789' & alt == "T")|
       (start == '32376790')|
       (start == '32376791' & alt == "C")|
       (start == '32376792')|
       (start == '32376793')
    ))
}

filter_avengers_ex22 = function(data, start, alt){
  data%>%filter(
    (start == '32379315' |
       (start == '32379316')|
       (start == '32379322' & alt %in% c("G", "A"))|
       (start == '32379324' & alt == "G")|
       (start == '32379329' & alt == "T")|
       (start == '32379332' & alt == "T")|
       (start == '32379335' & alt %in% c("T", "G"))|
       (start == '32379337' & alt == "C")|
       (start == '32379339' & alt %in% c("G", "A"))|
       (start == '32379341' & alt == "T")|
       (start == '32379348' & alt == "A")|
       (start == '32379359' & alt == "T")|
       (start == '32379362' & alt == "T")|
       (start == '32379369' & alt == "A")|
       (start == '32379377' & alt == "T")|
       (start == '32379380' & alt == "T")|
       (start == '32379383' & alt == "T")|
       (start == '32379384' & alt == "T")|
       (start == '32379389' & alt == "T")|
       (start == '32379395' & alt == "T")|
       (start == '32379399' & alt == "A")|
       (start == '32379401' & alt == "T")|
       (start == '32379410')|
       (start == '32379414' & alt == "G")|
       (start == '32379419' & alt == "T")|
       (start == '32379428' & alt == "T")|
       (start == '32379431' & alt == "T")|
       (start == '32379434' & alt == "T")|
       (start == '32379437' & alt == "T")|
       (start == '32379440' & alt == "T")|
       (start == '32379447' & alt %in% c("G", "A"))|
       (start == '32379450' & alt %in% c("G", "A"))|
       (start == '32379452' & alt == "G")|
       (start == '32379450' & alt %in% c("T", "G"))|
       (start == '32379471' & alt == "A")|
       (start == '32379472' & alt %in% c("T", "A"))|
       (start == '32379473' & alt == "T")|
       (start == '32379477' & alt == "A")|
       (start == '32379480' & alt == "C")|
       (start == '32379493' & alt %in% c("G", "A"))|
       (start == '32379495' & alt %in% c("G", "A"))|
       (start == '32379497' & alt == "T")|
       (start == '32379500' & alt == "T")|
       (start == '32379503' & alt == "T")|
       (start == '32379506' & alt == "T")|
       (start == '32379513' & alt %in% c("G", "A"))|
       (start == '32379515' & alt == "T")|
       (start == '32379516')|
       (start == '32379517')
    ))
}

filter_avengers_ex23 = function(data, start, alt){
  data%>%filter(
    ((start == '32379747' & alt %in% c("T", "G"))|
       (start == '32379748')|
       (start == '32379749')|
       (start == '32379756')|
       (start == '32379764' & alt == "A")|
       (start == '32379765' & alt %in% c("T", "A"))|
       (start == '32379766' & alt == "A")|
       (start == '32379768' & alt == "T")|
       (start == '32379770' & alt == "T")|
       (start == '32379771' & alt %in% c("T", "A"))|
       (start == '32379774' & alt %in% c("G", "A"))|
       (start == '32379777' & alt %in% c("G", "A"))|
       (start == '32379579' & alt == "C")|
       (start == '32379780' & alt == "T")|
       (start == '32379783' & alt %in% c("G", "A"))|
       (start == '32379787' & alt == "G")|
       (start == '32379791' & alt == "G")|
       (start == '32379792' & alt %in% c("G", "A"))|
       (start == '32379795' & alt %in% c("G", "A"))|
       (start == '32379797' & alt == "C")|
       (start == '32379799' & alt %in% c("T", "C"))|
       (start == '32379800')|
       (start == '32379801' & alt == "T")|
       (start == '32379802' & alt %in% c("T", "C"))|
       (start == '32379803' & alt == "C")|
       (start == '32379804' & alt == "C")|
       (start == '32379806' & alt == "T")|
       (start == '32379809' & alt == "T")|
       (start == '32379810' & alt == "T")|
       (start == '32379811' & alt == "C")|
       (start == '32379812')|
       (start == '32379813' & alt == "C")|
       (start == '32379814' & alt %in% c("G", "A"))|
       (start == '32379815' & alt %in% c("T", "G"))|
       (start == '32379816' & alt == "C")|
       (start == '32379817' & alt == "T")|
       (start == '32379819')|
       (start == '32379821')|
       (start == '32379823' & alt %in% c("G", "A"))|
       (start == '32379825' & alt == "G")|
       (start == '32379828' & alt %in% c("G", "A"))|
       (start == '32379876')|
       (start == '32379887' & alt == "T")|
       (start == '32379890' & alt == "T")|
       (start == '32379894' & alt == "A")|
       (start == '32379896' & alt == "T")|
       (start == '32379897' & alt == "C")|
       (start == '32379899' & alt %in% c("G", "A"))|
       (start == '32379901' & alt %in% c("G", "A"))|
       (start == '32379902' & alt == "T")|
       (start == '32379905' & alt == "T")|
       (start == '32379913' & alt == "T")|
       (start == '32379914')|
       (start == '32379915')
    ))
}

filter_avengers_ex24 = function(data, start, alt){
  data%>% 
    filter((start == '32380005')|
             (start == '32380006')|
             (start == '32380007' & alt == "T")|
             (start == '32380011' & alt %in% c("G", "A"))|
             (start == '32380016' & alt == "T")|
             (start == '32380023' & alt %in% c("G", "A"))|
             (start == '32380028' & alt == "T")|
             (start == '32380034' & alt %in% c("G", "A"))|
             (start == '32380035' & alt == "C")|
             (start == '32380036' & alt %in% c("G", "A"))|
             (start == '32380037' & alt == "T")|
             (start == '32380043' & alt %in% c("T", "G"))|
             (start == '32380044')|
             (start == '32380046' & alt == "T")|
             (start == '32380059' & alt %in% c("G", "C"))|
             (start == '32380064' & alt == "T")|
             (start == '32380071' & alt %in% c("G", "A"))|
             (start == '32380079' & alt == "T")|
             (start == '32380085' & alt == "T")|
             (start == '32380096' & alt == "A")|
             (start == '32380100' & alt %in% c("T", "A"))|
             (start == '32380104' & alt == "A")|
             (start == '32380106' & alt %in% c("T", "C"))|
             (start == '32380107' & alt %in% c("G", "C"))|
             (start == '32380110' & alt %in% c("G", "C"))|
             (start == '32380113' & alt %in% c("G", "A"))|
             (start == '32380115')|
             (start == '32380116' & alt == "T")|
             (start == '32380121' & alt == "C")|
             (start == '32380122' & alt == "A")|
             (start == '32380125' & alt %in% c("C", "A"))|
             (start == '32380127' & alt == "C")|
             (start == '32380128' & alt == "T")|
             (start == '32380131' & alt %in% c("G", "A"))|
             (start == '32380136' & alt == "T")|
             (start == '32380139' & alt == "T")|
             (start == '32380145' & alt %in% c("T", "A"))|
             (start == '32380146' & alt %in% c("C", "A"))|
             (start == '32380147' & alt %in% c("C", "A"))
    )
}

filter_avengers_ex25 = function(data, start, alt){
  data%>% 
    filter((start == '32394687')|
             (start == '32394688')|
             (start == '32394698' & alt == "T")|
             (start == '32394703' & alt == "T")|
             (start == '32394704' & alt %in% c("G", "A"))|
             (start == '32394706' & alt %in% c("G", "A"))|
             (start == '32394707' & alt == "C")|
             (start == '32394708' & alt %in% c("G", "A"))|
             (start == '32394712' & alt == "C")|
             (start == '32394713')|
             (start == '32394715' & alt == "C")|
             (start == '32394716' & alt == "T")|
             (start == '32394717' & alt %in% c("G", "A"))|
             (start == '32394718' & alt == "T")|
             (start == '32394723' & alt == "A")|
             (start == '32394726' & alt %in% c("G", "A"))|
             (start == '32394731')|
             (start == '32394734')|
             (start == '32394740' & alt %in% c("C", "A"))|
             (start == '32394742' & alt == "T")|
             (start == '32394749' & alt == "A")|
             (start == '32394750' & alt == "A")|
             (start == '32394763' & alt == "T")|
             (start == '32394773')|
             (start == '32394775' & alt == "T")|
             (start == '32394788' & alt %in% c("G", "A"))|
             (start == '32394791' & alt %in% c("G", "A"))|
             (start == '32394793' & alt == "C")|
             (start == '32394794' & alt == "A")|
             (start == '32394796' & alt == "C")|
             (start == '32394797' & alt == "A")|
             (start == '32394803' & alt %in% c("T", "C"))|
             (start == '32394805' & alt == "T")|
             (start == '32394806')|
             (start == '32394808' & alt == "T")|
             (start == '32394812' & alt == "A")|
             (start == '32394813' & alt == "A")|
             (start == '32394814' & alt == "T")|
             (start == '32394820' & alt == "T")|
             (start == '32394826' & alt == "T")|
             (start == '32394830' & alt %in% c("G", "A"))|
             (start == '32394839' & alt == "G")|
             (start == '32394841' & alt == "C")|
             (start == '32394845')|
             (start == '32394850' & alt == "C")|
             (start == '32394851' & alt %in% c("G", "A"))|
             (start == '32394853' & alt == "T")|
             (start == '32394854' & alt == "T")|
             (start == '32394862' & alt == "C")|
             (start == '32394863' & alt == "T")|
             (start == '32394869' & alt == "C")|
             (start == '32394871' & alt == "C")|
             (start == '32394881' & alt == "G")|
             (start == '32394883' & alt == "T")|
             (start == '32394886' & alt == "T")|
             (start == '32394898' & alt == "T")|
             (start == '32394901' & alt == "T")|
             (start == '32394908' & alt == "C")|
             (start == '32394913' & alt == "T")|
             (start == '32394919' & alt == "T")|
             (start == '32394931' & alt == "T")|
             (start == '32394932' & alt == "T")|
             (start == '32394934')|
             (start == '32394935')
    )
}

filter_avengers_ex26 = function(data, start, alt){
  data%>% 
    filter((start == '32396896')|
             (start == '32396897')|
             (start == '32396898' & alt == "T")|
             (start == '32396899' & alt %in% c("T", "C"))|  
             (start == '32396905' & alt %in% c("T", "C"))|
             (start == '32396911' & alt == "G")|
             (start == '32396913' & alt == "G")|
             (start == '32396915' & alt %in% c("G", "A"))|
             (start == '32396919' & alt == "T")|
             (start == '32396922' & alt == "A")|
             (start == '32394923' & alt %in% c("T", "A"))|
             (start == '32396925' & alt == "T")|
             (start == '32396931' & alt == "T")|
             (start == '32396935' & alt == "C")|
             (start == '32396940' & alt == "G")|
             (start == '32396964' & alt == "T")|
             (start == '32396968' & alt == "A")|
             (start == '32396969' & alt == "A")|
             (start == '32396982' & alt == "T")|
             (start == '32396990' & alt == "A")|
             (start == '32396995' & alt == "A")|
             (start == '32397005' & alt %in% c("G", "A"))|
             (start == '32397012' & alt == "T")|
             (start == '32397030' & alt == "T")|
             (start == '32397036' & alt == "T")|
             (start == '32397045' & alt %in% c("C", "A"))|
             (start == '32397046')
    )
}
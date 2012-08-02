module Const where


import StdLib
import Prime
import Data.List
import Data.Ratio


{-
 ----------------------------------------
 -                              RATIO
 ----------------------------------------
 -}



----------------------------------------------------------------------E2000------------------------------------------------------------------------------------

--don't need this because current int-log is fast enough, but nice, how one can transform a numer to difference formats to save it
-- the following has at least 5650 correct digits
r_e_2000 = (%) (39711763532951086374067282892665795731579919639519163524386226885212139760702033009611134294855425598601230590460943273159087306390612640697289219776354557837990982248914141798310111750519565743931979234745595228769341904541481790683971450875936702254822196618373064101425257846885151065119664925500348037812908088868225107521755163452107322135126455496958014241290825039115029349282707224839478129353770335431555800138720298286968959131630512786864017416639983657274716168943307880102294042693613465965958059300619459499014297050596576056213855115273649000206099893283256273677507887509796423968578794082351756396220818089175930333668848606640505528939777442659887414312021534944070706852800735598999253424057459731421547741699439285812854197966059673693249964437483098817056991900039102465682139115091119334403960903030593005026943223376835980593817735849238289923824770619926867277442734531258100274904088579836500958929555919718025982386849278955171291365339269199982585062796490985094514200504938019790392846885587199813202366828704120297472925784003368570696566744628592021530866891087108617559291464822041025652586504782727243723906608229677347893768911754433080666670651528696513891795160608117534210665919852810470912133261720528387313694247004245502242340272034064479843919391130848817993901587435967442928737442251426337381919589748487741514101220202520559669271711455268126175158591295108098388172224268961029430001900943330355776796177417832863474178744185946511034518696224065665517543719443573789028638651339884963464631833334293109186158923492683717663465673947785897925436467024709002876864446625677348821047335985810305800025162362089239001272610760121921794033523934586552600934613603625613959660237609506457785286386980598226213753753800425869502802798842442494922259367649362080190520036083991928090266753650786160345494260229089215713188595886292634908397361177835267524138361059152468774611939305844226194352080916827095557708801626458747926067651884645006508217207511964286123453831836904618785275588295380679335042938024845988395782621077120014394759344767138683210009802649007433551791180252209450865723794277463010025462319250008801528845759154485867242972779723578113968905707441726336565498757058325373226358942303068725066400135630161927302382941105651880909441108898073571103666363252456548502174409037807866161353204400314885096008528384893781792263803495792793666926913800722560505943208175950607799184820167047233766900779507982686063496652357787447457906207456913573215912455207568312711822401776914245216369434920703892090056344149168102494725263581007381573234262880763402290031414662499334607627279217942111073003784719928572340033534635138740610363214553485322233496651604955202825103896227586959144180411694271752811382369708125913145425237172440300103208275697419191251407838019411461438295244589065564640879958247446901075360088710762035165937926540555310950984254590856101131052685309620246353869648699213182209666996139531722506680489267287 
        * 10^2750 
        + 
        81357925106435123900795868948251336756542604808221696801227777280386251790205926586976070282147073467005388372224991812294921035935739347509176135197486150611871445768074121277172757878878969524788181853968255653202205425332795554002248878625751101773214599058927605256773740143544659943021618118754116061263841903895645109288661692039100633732932176874187206176774946934621396577583628662959909356492804669831810762071302975946837432002030369020543396613391594164365290024295377796312210338812494442252607649739915635200818510613381716142463856304575376634203098906020170685712474654551411841544221085737842761454764246343921551707743178575560025351744719149897916000789976632463407750257343970050808182481519274687687525576893852091384360556649209212111368908775193238857599366454774982959458708629932883910929467482168582012675605700703268468608994079367466074255294126728548122328393700292821006243594317099541373262654194631491425819958950168077700359043037825132895588627979042101368475482426850776991698023226178182292486216631860932471554740280004470517263062292073176780971799587368365627944759981586973299029821483191538138216018698518628653152814083738914142761020844164251703311118736650552612013321543956052722023557278246550053750166616445558347635107869445370602650924776595689337316264033750765161448725613102063707637694522984205313769119731135963361284953497385719818088030660832437693557408834923306147657781675810390394322474592641197243215478359264386555905778481862151321110343446026064256906099919693849528280089953636816373779069554844606009646384668929610444112604395178726013088750737352360796172361310825085826077812945678473678355114819488905356850851179578995552115868130742588544541741857491898261834216697173381779596470993436698916285985481970582660583352256102932979957541606514852433795602365744642250477200573857893318860385005393930918539447162715917924909372194980677466199575683755807878230646916393285854119790284269372607759557855647453656901281334758769243644528847017746809953415077589336588046246577764994421141715160829562359542043720779053787754614367310890217417992686106309501355281625822231938454074619276062655207130344031618160013117206944872961576310305681714140267552348920008039951292533971389721915940506589227041515388854629433059235191421850456447563792749811271390322466148347822649178203563589681041843776796778631565857777143321896097512156879715955687058398538182505524440869973961122772265745985192167352688111991010031534828689228838468956694385593592620593308545808770108379270045040442408972211166536928161894976140241045853565255871290646505394710362338849301575380778596716519686153853338061874602275624857837934322551778862315646407702491888468473294185666080960371117119211917489163)

        (pf_inv [(2,1994),(3,996),(5,499),(7,330),(11,198),(13,164),(17,123),(19,110),(23,89),(29,70),(31,66),(37,55),(41,49),(43,47),(47,42),(53,37),(59,33),(61,32),(67,29),(71,28),(73,27),(79,25),(83,24),(89,22),(97,20),(101,19),(103,19),(107,18),(109,18),(113,17),(127,15),(131,15),(137,14),(139,14),(149,13),(151,13),(157,12),(163,12),(167,11),(173,11),(179,11),(181,11),(191,10),(193,10),(197,10),(199,10),(211,9),(223,8),(227,7),(229,8),(233,8),(239,8),(241,8),(251,7),(257,7),(263,7),(269,7),(271,7),(277,7),(281,7),(283,7),(293,6),(307,6),(311,6),(313,6),(317,6),(331,6),(337,5),(347,5),(349,5),(353,5),(359,5),(367,5),(373,5),(379,5),(383,5),(389,5),(397,5),(401,4),(409,4),(419,4),(421,4),(431,4),(433,4),(439,4),(443,4),(449,4),(457,4),(461,4),(463,4),(467,4),(479,4),(487,4),(491,4),(499,4),(503,3),(509,3),(521,3),(523,3),(541,3),(547,3),(557,3),(563,3),(569,3),(571,3),(577,3),(587,3),(593,3),(599,3),(601,3),(607,3),(613,3),(617,3),(619,3),(631,3),(641,3),(643,3),(647,3),(653,3),(659,3),(661,3),(673,2),(677,2),(683,2),(691,2),(701,2),(709,2),(719,2),(727,2),(733,2),(739,2),(743,2),(751,2),(757,2),(761,2),(769,2),(773,2),(787,2),(797,2),(809,2),(811,2),(821,2),(823,2),(827,2),(829,2),(839,2),(853,2),(857,2),(859,2),(863,2),(877,2),(881,2),(883,2),(887,2),(907,2),(911,2),(919,2),(929,2),(937,2),(941,2),(947,2),(953,2),(967,2),(971,2),(977,2),(983,2),(991,2),(997,2)] *
        14771291995722167329218042370113255785101138530361517229308539087571387624569184860194674818433642463913959873342852255543111135151457209398370030102228858128758106619882182645353368949658870563576166323307466834284670054906895828663228915811739807554988946409285249055824035034910383070200873326731464685909980630202533115332294811841112677175465781631555282945350555352712075708686158031835188673039526232167090966030801645637)

----------------------------------------------------------------------E2000------------------------------------------------------------------------------------



{-
 ----------------------------------------
 -                              PRIMES
 ----------------------------------------
 -}



--some hard_coded primes for speed testings
big_prime 12    = 1000000000039
big_prime 13    = 10000000000037
big_prime 14    = 100000000000031
big_prime 15    = 1000000000000037
big_prime n     = next_prime (10^n)
fat_prime       = big_prime 15
fat_prime_succ  = 1000000000000091
--10^17
fat_fat_prime   = 100000000000000003
fat_fat_prime2  = 101111111111111111
--10^21 ?
--fat_fat_fat_prime



pipc180 = [3,7,12,26,34,55,65,91,137,152,208,251,270,315,394,471,502,591,656,685,790,864,977,1139,1227,1268,1354,1395,1494,1847,1945,2109,2157,2455,2512,2693,2878,3005,3202,3396,3471,3826,3902,4045,4119,4581,5059,5226,5307,5472,5741,5830,6267,6541,6814,7105,7195,7486,7684,7782,8293,9027,9246,9352,9566,10349,10689,11267,11391,11631,11993,12481,12861,13240,13491,13884,14413,14674,15204,15873,16023,16731,16881,17304,17593,18022,18620,18924,19068,19361,20294,20905,21217,21870,22187,22669,23663,23827,25339,25836,26719,27254,27800,27988,28527,29438,29996,30559,30726,31307,31862,32237,32425,33599,34566,34759,35161,35762,36384,36594,37812,38215,38827,39685,40749,41611,42693,43587,44252,44900,45350,46237,46931,47386,48311,48780,50431,51592,53017,53257,54473,54716,55213,55458,56691,58462,58966,59224,59713,61520,62031,62292,62815,65462,66001,67074,68421,69511,70062,70892,71718,73681,74236,75072,75917,77048,77893,79662,80257,81128,81417,82858,83161,84057,85572,85871,87395,87705,88613,91433]



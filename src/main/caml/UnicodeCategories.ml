(*
  Copyright 2012 Google, Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 *)

include DisableGenericCompare

module Range = Unicode.Range
module RangeSet = Unicode.Range.Set

let range s e = Unicode.Range.make (Unicode.i2uni s) (Unicode.i2uni e)

module SMap = Map.Make(String)

let category_map = (
  let categories = SMap.add "Cc" (RangeSet.make [
    range 0 32; range 127 160;
  ]) SMap.empty in
  let categories = SMap.add "Zs" (RangeSet.make [
    range 32 33; range 160 161; range 5760 5761;
    range 6158 6159; range 8192 8203; range 8239 8240;
    range 8287 8288; range 12288 12289;
  ]) categories in
  let categories = SMap.add "Po" (RangeSet.make [
    range 33 36; range 37 40; range 42 43; range 44 45;
    range 46 48; range 58 60; range 63 65; range 92 93;
    range 161 162; range 183 184; range 191 192;
    range 894 895; range 903 904; range 1370 1376;
    range 1417 1418; range 1470 1471; range 1472 1473;
    range 1475 1476; range 1523 1525; range 1548 1550;
    range 1563 1564; range 1567 1568; range 1642 1646;
    range 1748 1749; range 1792 1806; range 2404 2406;
    range 2416 2417; range 3572 3573; range 3663 3664;
    range 3674 3676; range 3844 3859; range 3973 3974;
    range 4170 4176; range 4347 4348; range 4961 4969;
    range 5741 5743; range 5867 5870; range 5941 5943;
    range 6100 6103; range 6104 6107; range 6144 6150;
    range 6151 6155; range 6468 6470; range 8214 8216;
    range 8224 8232; range 8240 8249; range 8251 8255;
    range 8257 8260; range 8263 8274; range 8275 8276;
    range 8279 8280; range 9142 9143; range 12289 12292;
    range 12349 12350; range 65072 65073; range 65093 65095;
    range 65097 65101; range 65104 65107; range 65108 65112;
    range 65119 65122; range 65128 65129; range 65130 65132;
    range 65281 65284; range 65285 65288; range 65290 65291;
    range 65292 65293; range 65294 65296; range 65306 65308;
    range 65311 65313; range 65340 65341; range 65377 65378;
    range 65380 65381;
  ]) categories in
  let categories = SMap.add "Sc" (RangeSet.make [
    range 36 37; range 162 166; range 2546 2548;
    range 2801 2802; range 3065 3066; range 3647 3648;
    range 6107 6108; range 8352 8370; range 65020 65021;
    range 65129 65130; range 65284 65285; range 65504 65506;
    range 65509 65511;
  ]) categories in
  let categories = SMap.add "Ps" (RangeSet.make [
    range 40 41; range 91 92; range 123 124;
    range 3898 3899; range 3900 3901; range 5787 5788;
    range 8261 8262; range 8317 8318; range 8333 8334;
    range 9001 9002; range 9140 9141; range 10088 10089;
    range 10090 10091; range 10092 10093; range 10094 10095;
    range 10096 10097; range 10098 10099; range 10100 10101;
    range 10214 10215; range 10216 10217; range 10218 10219;
    range 10627 10628; range 10629 10630; range 10631 10632;
    range 10633 10634; range 10635 10636; range 10637 10638;
    range 10639 10640; range 10641 10642; range 10643 10644;
    range 10645 10646; range 10647 10648; range 10712 10713;
    range 10714 10715; range 10748 10749; range 12296 12297;
    range 12298 12299; range 12300 12301; range 12302 12303;
    range 12304 12305; range 12308 12309; range 12310 12311;
    range 12312 12313; range 12314 12315; range 12317 12318;
    range 64830 64831; range 65077 65078; range 65079 65080;
    range 65081 65082; range 65083 65084; range 65085 65086;
    range 65087 65088; range 65089 65090; range 65091 65092;
    range 65095 65096; range 65113 65114; range 65115 65116;
    range 65117 65118; range 65288 65289; range 65339 65340;
    range 65371 65372; range 65375 65376; range 65378 65379;
  ]) categories in
  let categories = SMap.add "Pe" (RangeSet.make [
    range 41 42; range 93 94; range 125 126;
    range 3899 3900; range 3901 3902; range 5788 5789;
    range 8262 8263; range 8318 8319; range 8334 8335;
    range 9002 9003; range 9141 9142; range 10089 10090;
    range 10091 10092; range 10093 10094; range 10095 10096;
    range 10097 10098; range 10099 10100; range 10101 10102;
    range 10215 10216; range 10217 10218; range 10219 10220;
    range 10628 10629; range 10630 10631; range 10632 10633;
    range 10634 10635; range 10636 10637; range 10638 10639;
    range 10640 10641; range 10642 10643; range 10644 10645;
    range 10646 10647; range 10648 10649; range 10713 10714;
    range 10715 10716; range 10749 10750; range 12297 12298;
    range 12299 12300; range 12301 12302; range 12303 12304;
    range 12305 12306; range 12309 12310; range 12311 12312;
    range 12313 12314; range 12315 12316; range 12318 12320;
    range 64831 64832; range 65078 65079; range 65080 65081;
    range 65082 65083; range 65084 65085; range 65086 65087;
    range 65088 65089; range 65090 65091; range 65092 65093;
    range 65096 65097; range 65114 65115; range 65116 65117;
    range 65118 65119; range 65289 65290; range 65341 65342;
    range 65373 65374; range 65376 65377; range 65379 65380;
  ]) categories in
  let categories = SMap.add "Sm" (RangeSet.make [
    range 43 44; range 60 63; range 124 125; range 126 127;
    range 172 173; range 177 178; range 215 216;
    range 247 248; range 1014 1015; range 8260 8261;
    range 8274 8275; range 8314 8317; range 8330 8333;
    range 8512 8517; range 8523 8524; range 8592 8597;
    range 8602 8604; range 8608 8609; range 8611 8612;
    range 8614 8615; range 8622 8623; range 8654 8656;
    range 8658 8659; range 8660 8661; range 8692 8960;
    range 8968 8972; range 8992 8994; range 9084 9085;
    range 9115 9140; range 9655 9656; range 9665 9666;
    range 9720 9728; range 9839 9840; range 10192 10214;
    range 10224 10240; range 10496 10627; range 10649 10712;
    range 10716 10748; range 10750 11008; range 64297 64298;
    range 65122 65123; range 65124 65127; range 65291 65292;
    range 65308 65311; range 65372 65373; range 65374 65375;
    range 65506 65507; range 65513 65517;
  ]) categories in
  let categories = SMap.add "Pd" (RangeSet.make [
    range 45 46; range 1418 1419; range 6150 6151;
    range 8208 8214; range 12316 12317; range 12336 12337;
    range 12448 12449; range 65073 65075; range 65112 65113;
    range 65123 65124; range 65293 65294;
  ]) categories in
  let categories = SMap.add "Nd" (RangeSet.make [
    range 48 58; range 1632 1642; range 1776 1786;
    range 2406 2416; range 2534 2544; range 2662 2672;
    range 2790 2800; range 2918 2928; range 3047 3056;
    range 3174 3184; range 3302 3312; range 3430 3440;
    range 3664 3674; range 3792 3802; range 3872 3882;
    range 4160 4170; range 4969 4978; range 6112 6122;
    range 6160 6170; range 6470 6480; range 65296 65306;
  ]) categories in
  let categories = SMap.add "Lu" (RangeSet.make [
    range 65 91; range 192 215; range 216 223;
    range 256 257; range 258 259; range 260 261;
    range 262 263; range 264 265; range 266 267;
    range 268 269; range 270 271; range 272 273;
    range 274 275; range 276 277; range 278 279;
    range 280 281; range 282 283; range 284 285;
    range 286 287; range 288 289; range 290 291;
    range 292 293; range 294 295; range 296 297;
    range 298 299; range 300 301; range 302 303;
    range 304 305; range 306 307; range 308 309;
    range 310 311; range 313 314; range 315 316;
    range 317 318; range 319 320; range 321 322;
    range 323 324; range 325 326; range 327 328;
    range 330 331; range 332 333; range 334 335;
    range 336 337; range 338 339; range 340 341;
    range 342 343; range 344 345; range 346 347;
    range 348 349; range 350 351; range 352 353;
    range 354 355; range 356 357; range 358 359;
    range 360 361; range 362 363; range 364 365;
    range 366 367; range 368 369; range 370 371;
    range 372 373; range 374 375; range 376 378;
    range 379 380; range 381 382; range 385 387;
    range 388 389; range 390 392; range 393 396;
    range 398 402; range 403 405; range 406 409;
    range 412 414; range 415 417; range 418 419;
    range 420 421; range 422 424; range 425 426;
    range 428 429; range 430 432; range 433 436;
    range 437 438; range 439 441; range 444 445;
    range 452 453; range 455 456; range 458 459;
    range 461 462; range 463 464; range 465 466;
    range 467 468; range 469 470; range 471 472;
    range 473 474; range 475 476; range 478 479;
    range 480 481; range 482 483; range 484 485;
    range 486 487; range 488 489; range 490 491;
    range 492 493; range 494 495; range 497 498;
    range 500 501; range 502 505; range 506 507;
    range 508 509; range 510 511; range 512 513;
    range 514 515; range 516 517; range 518 519;
    range 520 521; range 522 523; range 524 525;
    range 526 527; range 528 529; range 530 531;
    range 532 533; range 534 535; range 536 537;
    range 538 539; range 540 541; range 542 543;
    range 544 545; range 546 547; range 548 549;
    range 550 551; range 552 553; range 554 555;
    range 556 557; range 558 559; range 560 561;
    range 562 563; range 902 903; range 904 907;
    range 908 909; range 910 912; range 913 930;
    range 931 940; range 978 981; range 984 985;
    range 986 987; range 988 989; range 990 991;
    range 992 993; range 994 995; range 996 997;
    range 998 999; range 1000 1001; range 1002 1003;
    range 1004 1005; range 1006 1007; range 1012 1013;
    range 1015 1016; range 1017 1019; range 1024 1072;
    range 1120 1121; range 1122 1123; range 1124 1125;
    range 1126 1127; range 1128 1129; range 1130 1131;
    range 1132 1133; range 1134 1135; range 1136 1137;
    range 1138 1139; range 1140 1141; range 1142 1143;
    range 1144 1145; range 1146 1147; range 1148 1149;
    range 1150 1151; range 1152 1153; range 1162 1163;
    range 1164 1165; range 1166 1167; range 1168 1169;
    range 1170 1171; range 1172 1173; range 1174 1175;
    range 1176 1177; range 1178 1179; range 1180 1181;
    range 1182 1183; range 1184 1185; range 1186 1187;
    range 1188 1189; range 1190 1191; range 1192 1193;
    range 1194 1195; range 1196 1197; range 1198 1199;
    range 1200 1201; range 1202 1203; range 1204 1205;
    range 1206 1207; range 1208 1209; range 1210 1211;
    range 1212 1213; range 1214 1215; range 1216 1218;
    range 1219 1220; range 1221 1222; range 1223 1224;
    range 1225 1226; range 1227 1228; range 1229 1230;
    range 1232 1233; range 1234 1235; range 1236 1237;
    range 1238 1239; range 1240 1241; range 1242 1243;
    range 1244 1245; range 1246 1247; range 1248 1249;
    range 1250 1251; range 1252 1253; range 1254 1255;
    range 1256 1257; range 1258 1259; range 1260 1261;
    range 1262 1263; range 1264 1265; range 1266 1267;
    range 1268 1269; range 1272 1273; range 1280 1281;
    range 1282 1283; range 1284 1285; range 1286 1287;
    range 1288 1289; range 1290 1291; range 1292 1293;
    range 1294 1295; range 1329 1367; range 4256 4294;
    range 7680 7681; range 7682 7683; range 7684 7685;
    range 7686 7687; range 7688 7689; range 7690 7691;
    range 7692 7693; range 7694 7695; range 7696 7697;
    range 7698 7699; range 7700 7701; range 7702 7703;
    range 7704 7705; range 7706 7707; range 7708 7709;
    range 7710 7711; range 7712 7713; range 7714 7715;
    range 7716 7717; range 7718 7719; range 7720 7721;
    range 7722 7723; range 7724 7725; range 7726 7727;
    range 7728 7729; range 7730 7731; range 7732 7733;
    range 7734 7735; range 7736 7737; range 7738 7739;
    range 7740 7741; range 7742 7743; range 7744 7745;
    range 7746 7747; range 7748 7749; range 7750 7751;
    range 7752 7753; range 7754 7755; range 7756 7757;
    range 7758 7759; range 7760 7761; range 7762 7763;
    range 7764 7765; range 7766 7767; range 7768 7769;
    range 7770 7771; range 7772 7773; range 7774 7775;
    range 7776 7777; range 7778 7779; range 7780 7781;
    range 7782 7783; range 7784 7785; range 7786 7787;
    range 7788 7789; range 7790 7791; range 7792 7793;
    range 7794 7795; range 7796 7797; range 7798 7799;
    range 7800 7801; range 7802 7803; range 7804 7805;
    range 7806 7807; range 7808 7809; range 7810 7811;
    range 7812 7813; range 7814 7815; range 7816 7817;
    range 7818 7819; range 7820 7821; range 7822 7823;
    range 7824 7825; range 7826 7827; range 7828 7829;
    range 7840 7841; range 7842 7843; range 7844 7845;
    range 7846 7847; range 7848 7849; range 7850 7851;
    range 7852 7853; range 7854 7855; range 7856 7857;
    range 7858 7859; range 7860 7861; range 7862 7863;
    range 7864 7865; range 7866 7867; range 7868 7869;
    range 7870 7871; range 7872 7873; range 7874 7875;
    range 7876 7877; range 7878 7879; range 7880 7881;
    range 7882 7883; range 7884 7885; range 7886 7887;
    range 7888 7889; range 7890 7891; range 7892 7893;
    range 7894 7895; range 7896 7897; range 7898 7899;
    range 7900 7901; range 7902 7903; range 7904 7905;
    range 7906 7907; range 7908 7909; range 7910 7911;
    range 7912 7913; range 7914 7915; range 7916 7917;
    range 7918 7919; range 7920 7921; range 7922 7923;
    range 7924 7925; range 7926 7927; range 7928 7929;
    range 7944 7952; range 7960 7966; range 7976 7984;
    range 7992 8000; range 8008 8014; range 8025 8026;
    range 8027 8028; range 8029 8030; range 8031 8032;
    range 8040 8048; range 8120 8124; range 8136 8140;
    range 8152 8156; range 8168 8173; range 8184 8188;
    range 8450 8451; range 8455 8456; range 8459 8462;
    range 8464 8467; range 8469 8470; range 8473 8478;
    range 8484 8485; range 8486 8487; range 8488 8489;
    range 8490 8494; range 8496 8498; range 8499 8500;
    range 8510 8512; range 8517 8518; range 65313 65339;
  ]) categories in
  let categories = SMap.add "Sk" (RangeSet.make [
    range 94 95; range 96 97; range 168 169; range 175 176;
    range 180 181; range 184 185; range 706 710;
    range 722 736; range 741 750; range 751 768;
    range 884 886; range 900 902; range 8125 8126;
    range 8127 8130; range 8141 8144; range 8157 8160;
    range 8173 8176; range 8189 8191; range 12443 12445;
    range 65342 65343; range 65344 65345; range 65507 65508;
  ]) categories in
  let categories = SMap.add "Pc" (RangeSet.make [
    range 95 96; range 8255 8257; range 8276 8277;
    range 12539 12540; range 65075 65077; range 65101 65104;
    range 65343 65344; range 65381 65382;
  ]) categories in
  let categories = SMap.add "Ll" (RangeSet.make [
    range 97 123; range 170 171; range 181 182;
    range 186 187; range 223 247; range 248 256;
    range 257 258; range 259 260; range 261 262;
    range 263 264; range 265 266; range 267 268;
    range 269 270; range 271 272; range 273 274;
    range 275 276; range 277 278; range 279 280;
    range 281 282; range 283 284; range 285 286;
    range 287 288; range 289 290; range 291 292;
    range 293 294; range 295 296; range 297 298;
    range 299 300; range 301 302; range 303 304;
    range 305 306; range 307 308; range 309 310;
    range 311 313; range 314 315; range 316 317;
    range 318 319; range 320 321; range 322 323;
    range 324 325; range 326 327; range 328 330;
    range 331 332; range 333 334; range 335 336;
    range 337 338; range 339 340; range 341 342;
    range 343 344; range 345 346; range 347 348;
    range 349 350; range 351 352; range 353 354;
    range 355 356; range 357 358; range 359 360;
    range 361 362; range 363 364; range 365 366;
    range 367 368; range 369 370; range 371 372;
    range 373 374; range 375 376; range 378 379;
    range 380 381; range 382 385; range 387 388;
    range 389 390; range 392 393; range 396 398;
    range 402 403; range 405 406; range 409 412;
    range 414 415; range 417 418; range 419 420;
    range 421 422; range 424 425; range 426 428;
    range 429 430; range 432 433; range 436 437;
    range 438 439; range 441 443; range 445 448;
    range 454 455; range 457 458; range 460 461;
    range 462 463; range 464 465; range 466 467;
    range 468 469; range 470 471; range 472 473;
    range 474 475; range 476 478; range 479 480;
    range 481 482; range 483 484; range 485 486;
    range 487 488; range 489 490; range 491 492;
    range 493 494; range 495 497; range 499 500;
    range 501 502; range 505 506; range 507 508;
    range 509 510; range 511 512; range 513 514;
    range 515 516; range 517 518; range 519 520;
    range 521 522; range 523 524; range 525 526;
    range 527 528; range 529 530; range 531 532;
    range 533 534; range 535 536; range 537 538;
    range 539 540; range 541 542; range 543 544;
    range 545 546; range 547 548; range 549 550;
    range 551 552; range 553 554; range 555 556;
    range 557 558; range 559 560; range 561 562;
    range 563 567; range 592 688; range 912 913;
    range 940 975; range 976 978; range 981 984;
    range 985 986; range 987 988; range 989 990;
    range 991 992; range 993 994; range 995 996;
    range 997 998; range 999 1000; range 1001 1002;
    range 1003 1004; range 1005 1006; range 1007 1012;
    range 1013 1014; range 1016 1017; range 1019 1020;
    range 1072 1120; range 1121 1122; range 1123 1124;
    range 1125 1126; range 1127 1128; range 1129 1130;
    range 1131 1132; range 1133 1134; range 1135 1136;
    range 1137 1138; range 1139 1140; range 1141 1142;
    range 1143 1144; range 1145 1146; range 1147 1148;
    range 1149 1150; range 1151 1152; range 1153 1154;
    range 1163 1164; range 1165 1166; range 1167 1168;
    range 1169 1170; range 1171 1172; range 1173 1174;
    range 1175 1176; range 1177 1178; range 1179 1180;
    range 1181 1182; range 1183 1184; range 1185 1186;
    range 1187 1188; range 1189 1190; range 1191 1192;
    range 1193 1194; range 1195 1196; range 1197 1198;
    range 1199 1200; range 1201 1202; range 1203 1204;
    range 1205 1206; range 1207 1208; range 1209 1210;
    range 1211 1212; range 1213 1214; range 1215 1216;
    range 1218 1219; range 1220 1221; range 1222 1223;
    range 1224 1225; range 1226 1227; range 1228 1229;
    range 1230 1231; range 1233 1234; range 1235 1236;
    range 1237 1238; range 1239 1240; range 1241 1242;
    range 1243 1244; range 1245 1246; range 1247 1248;
    range 1249 1250; range 1251 1252; range 1253 1254;
    range 1255 1256; range 1257 1258; range 1259 1260;
    range 1261 1262; range 1263 1264; range 1265 1266;
    range 1267 1268; range 1269 1270; range 1273 1274;
    range 1281 1282; range 1283 1284; range 1285 1286;
    range 1287 1288; range 1289 1290; range 1291 1292;
    range 1293 1294; range 1295 1296; range 1377 1416;
    range 7424 7468; range 7522 7532; range 7681 7682;
    range 7683 7684; range 7685 7686; range 7687 7688;
    range 7689 7690; range 7691 7692; range 7693 7694;
    range 7695 7696; range 7697 7698; range 7699 7700;
    range 7701 7702; range 7703 7704; range 7705 7706;
    range 7707 7708; range 7709 7710; range 7711 7712;
    range 7713 7714; range 7715 7716; range 7717 7718;
    range 7719 7720; range 7721 7722; range 7723 7724;
    range 7725 7726; range 7727 7728; range 7729 7730;
    range 7731 7732; range 7733 7734; range 7735 7736;
    range 7737 7738; range 7739 7740; range 7741 7742;
    range 7743 7744; range 7745 7746; range 7747 7748;
    range 7749 7750; range 7751 7752; range 7753 7754;
    range 7755 7756; range 7757 7758; range 7759 7760;
    range 7761 7762; range 7763 7764; range 7765 7766;
    range 7767 7768; range 7769 7770; range 7771 7772;
    range 7773 7774; range 7775 7776; range 7777 7778;
    range 7779 7780; range 7781 7782; range 7783 7784;
    range 7785 7786; range 7787 7788; range 7789 7790;
    range 7791 7792; range 7793 7794; range 7795 7796;
    range 7797 7798; range 7799 7800; range 7801 7802;
    range 7803 7804; range 7805 7806; range 7807 7808;
    range 7809 7810; range 7811 7812; range 7813 7814;
    range 7815 7816; range 7817 7818; range 7819 7820;
    range 7821 7822; range 7823 7824; range 7825 7826;
    range 7827 7828; range 7829 7836; range 7841 7842;
    range 7843 7844; range 7845 7846; range 7847 7848;
    range 7849 7850; range 7851 7852; range 7853 7854;
    range 7855 7856; range 7857 7858; range 7859 7860;
    range 7861 7862; range 7863 7864; range 7865 7866;
    range 7867 7868; range 7869 7870; range 7871 7872;
    range 7873 7874; range 7875 7876; range 7877 7878;
    range 7879 7880; range 7881 7882; range 7883 7884;
    range 7885 7886; range 7887 7888; range 7889 7890;
    range 7891 7892; range 7893 7894; range 7895 7896;
    range 7897 7898; range 7899 7900; range 7901 7902;
    range 7903 7904; range 7905 7906; range 7907 7908;
    range 7909 7910; range 7911 7912; range 7913 7914;
    range 7915 7916; range 7917 7918; range 7919 7920;
    range 7921 7922; range 7923 7924; range 7925 7926;
    range 7927 7928; range 7929 7930; range 7936 7944;
    range 7952 7958; range 7968 7976; range 7984 7992;
    range 8000 8006; range 8016 8024; range 8032 8040;
    range 8048 8062; range 8064 8072; range 8080 8088;
    range 8096 8104; range 8112 8117; range 8118 8120;
    range 8126 8127; range 8130 8133; range 8134 8136;
    range 8144 8148; range 8150 8152; range 8160 8168;
    range 8178 8181; range 8182 8184; range 8305 8306;
    range 8319 8320; range 8458 8459; range 8462 8464;
    range 8467 8468; range 8495 8496; range 8500 8501;
    range 8505 8506; range 8509 8510; range 8518 8522;
    range 64256 64263; range 64275 64280; range 65345 65371;
  ]) categories in
  let categories = SMap.add "So" (RangeSet.make [
    range 166 168; range 169 170; range 174 175;
    range 176 177; range 182 183; range 1154 1155;
    range 1550 1552; range 1769 1770; range 1789 1791;
    range 2554 2555; range 2928 2929; range 3059 3065;
    range 3066 3067; range 3841 3844; range 3859 3864;
    range 3866 3872; range 3892 3893; range 3894 3895;
    range 3896 3897; range 4030 4038; range 4039 4045;
    range 4047 4048; range 6464 6465; range 6624 6656;
    range 8448 8450; range 8451 8455; range 8456 8458;
    range 8468 8469; range 8470 8473; range 8478 8484;
    range 8485 8486; range 8487 8488; range 8489 8490;
    range 8494 8495; range 8498 8499; range 8506 8508;
    range 8522 8523; range 8597 8602; range 8604 8608;
    range 8609 8611; range 8612 8614; range 8615 8622;
    range 8623 8654; range 8656 8658; range 8659 8660;
    range 8661 8692; range 8960 8968; range 8972 8992;
    range 8994 9001; range 9003 9084; range 9085 9115;
    range 9143 9146; range 9150 9169; range 9216 9255;
    range 9280 9291; range 9372 9450; range 9472 9655;
    range 9656 9665; range 9666 9720; range 9728 9752;
    range 9753 9839; range 9840 9843; range 9850 9854;
    range 9862 9874; range 9888 9890; range 9985 9989;
    range 9990 9994; range 9996 10024; range 10025 10060;
    range 10061 10062; range 10063 10067; range 10070 10071;
    range 10072 10079; range 10081 10088; range 10132 10133;
    range 10136 10160; range 10161 10175; range 10240 10241;
    range 11008 11022; range 11904 11930; range 11931 12020;
    range 12032 12246; range 12272 12284; range 12292 12293;
    range 12306 12308; range 12320 12321; range 12342 12344;
    range 12350 12352; range 12688 12690; range 12694 12704;
    range 12800 12831; range 12842 12868; range 12880 12881;
    range 12896 12926; range 12927 12928; range 12938 12977;
    range 12992 13055; range 13056 13312; range 19904 19968;
    range 42128 42183; range 65021 65022; range 65508 65509;
    range 65512 65513; range 65517 65519; range 65532 65534;
  ]) categories in
  let categories = SMap.add "Pi" (RangeSet.make [
    range 171 172; range 8216 8217; range 8220 8221;
    range 8249 8250;
  ]) categories in
  let categories = SMap.add "Cf" (RangeSet.make [
    range 173 174; range 1536 1540; range 1757 1758;
    range 1807 1808; range 6068 6070; range 8203 8208;
    range 8234 8239; range 8288 8292; range 8298 8304;
    range 65279 65280; range 65529 65532;
  ]) categories in
  let categories = SMap.add "No" (RangeSet.make [
    range 178 180; range 185 186; range 188 191;
    range 2548 2554; range 3056 3059; range 3882 3892;
    range 4978 4989; range 6128 6138; range 8304 8305;
    range 8308 8314; range 8320 8330; range 8531 8544;
    range 9312 9372; range 9450 9472; range 10102 10132;
    range 12690 12694; range 12832 12842; range 12881 12896;
    range 12928 12938; range 12977 12992;
  ]) categories in
  let categories = SMap.add "Pf" (RangeSet.make [
    range 187 188; range 8217 8218; range 8221 8222;
    range 8250 8251;
  ]) categories in
  let categories = SMap.add "Lo" (RangeSet.make [
    range 443 444; range 448 452; range 1488 1515;
    range 1520 1523; range 1569 1595; range 1601 1611;
    range 1646 1648; range 1649 1748; range 1749 1750;
    range 1774 1776; range 1786 1789; range 1791 1792;
    range 1808 1809; range 1810 1840; range 1869 1872;
    range 1920 1958; range 1969 1970; range 2308 2362;
    range 2365 2366; range 2384 2385; range 2392 2402;
    range 2437 2445; range 2447 2449; range 2451 2473;
    range 2474 2481; range 2482 2483; range 2486 2490;
    range 2493 2494; range 2524 2526; range 2527 2530;
    range 2544 2546; range 2565 2571; range 2575 2577;
    range 2579 2601; range 2602 2609; range 2610 2612;
    range 2613 2615; range 2616 2618; range 2649 2653;
    range 2654 2655; range 2674 2677; range 2693 2702;
    range 2703 2706; range 2707 2729; range 2730 2737;
    range 2738 2740; range 2741 2746; range 2749 2750;
    range 2768 2769; range 2784 2786; range 2821 2829;
    range 2831 2833; range 2835 2857; range 2858 2865;
    range 2866 2868; range 2869 2874; range 2877 2878;
    range 2908 2910; range 2911 2914; range 2929 2930;
    range 2947 2948; range 2949 2955; range 2958 2961;
    range 2962 2966; range 2969 2971; range 2972 2973;
    range 2974 2976; range 2979 2981; range 2984 2987;
    range 2990 2998; range 2999 3002; range 3077 3085;
    range 3086 3089; range 3090 3113; range 3114 3124;
    range 3125 3130; range 3168 3170; range 3205 3213;
    range 3214 3217; range 3218 3241; range 3242 3252;
    range 3253 3258; range 3261 3262; range 3294 3295;
    range 3296 3298; range 3333 3341; range 3342 3345;
    range 3346 3369; range 3370 3386; range 3424 3426;
    range 3461 3479; range 3482 3506; range 3507 3516;
    range 3517 3518; range 3520 3527; range 3585 3633;
    range 3634 3636; range 3648 3654; range 3713 3715;
    range 3716 3717; range 3719 3721; range 3722 3723;
    range 3725 3726; range 3732 3736; range 3737 3744;
    range 3745 3748; range 3749 3750; range 3751 3752;
    range 3754 3756; range 3757 3761; range 3762 3764;
    range 3773 3774; range 3776 3781; range 3804 3806;
    range 3840 3841; range 3904 3912; range 3913 3947;
    range 3976 3980; range 4096 4130; range 4131 4136;
    range 4137 4139; range 4176 4182; range 4304 4345;
    range 4352 4442; range 4447 4515; range 4520 4602;
    range 4608 4615; range 4616 4679; range 4680 4681;
    range 4682 4686; range 4688 4695; range 4696 4697;
    range 4698 4702; range 4704 4743; range 4744 4745;
    range 4746 4750; range 4752 4783; range 4784 4785;
    range 4786 4790; range 4792 4799; range 4800 4801;
    range 4802 4806; range 4808 4815; range 4816 4823;
    range 4824 4847; range 4848 4879; range 4880 4881;
    range 4882 4886; range 4888 4895; range 4896 4935;
    range 4936 4955; range 5024 5109; range 5121 5741;
    range 5743 5751; range 5761 5787; range 5792 5867;
    range 5888 5901; range 5902 5906; range 5920 5938;
    range 5952 5970; range 5984 5997; range 5998 6001;
    range 6016 6068; range 6108 6109; range 6176 6211;
    range 6212 6264; range 6272 6313; range 6400 6429;
    range 6480 6510; range 8501 8505; range 12294 12295;
    range 12348 12349; range 12353 12439; range 12447 12448;
    range 12449 12539; range 12543 12544; range 12549 12589;
    range 12593 12687; range 12704 12728; range 12784 12800;
    range 13312 13313; range 19893 19894; range 19968 19969;
    range 40869 40870; range 40960 42125; range 44032 44033;
    range 55203 55204; range 64285 64286; range 64287 64297;
    range 64298 64311; range 64312 64317; range 64318 64319;
    range 64320 64322; range 64323 64325; range 64326 64434;
    range 64467 64830; range 64848 64912; range 64914 64968;
    range 65008 65020; range 65136 65141; range 65142 65277;
    range 65382 65392; range 65393 65438; range 65440 65471;
    range 65474 65480; range 65482 65488; range 65490 65496;
    range 65498 65501;
  ]) categories in
  let categories = SMap.add "Lt" (RangeSet.make [
    range 453 454; range 456 457; range 459 460;
    range 498 499; range 8072 8080; range 8088 8096;
    range 8104 8112; range 8124 8125; range 8140 8141;
    range 8188 8189;
  ]) categories in
  let categories = SMap.add "Lm" (RangeSet.make [
    range 688 706; range 710 722; range 736 741;
    range 750 751; range 890 891; range 1369 1370;
    range 1600 1601; range 1765 1767; range 3654 3655;
    range 3782 3783; range 6103 6104; range 6211 6212;
    range 7468 7522; range 12293 12294; range 12337 12342;
    range 12347 12348; range 12445 12447; range 12540 12543;
    range 65392 65393; range 65438 65440;
  ]) categories in
  let categories = SMap.add "Mn" (RangeSet.make [
    range 768 856; range 861 880; range 1155 1159;
    range 1425 1442; range 1443 1466; range 1467 1470;
    range 1471 1472; range 1473 1475; range 1476 1477;
    range 1552 1558; range 1611 1625; range 1648 1649;
    range 1750 1757; range 1759 1765; range 1767 1769;
    range 1770 1774; range 1809 1810; range 1840 1867;
    range 1958 1969; range 2305 2307; range 2364 2365;
    range 2369 2377; range 2381 2382; range 2385 2389;
    range 2402 2404; range 2433 2434; range 2492 2493;
    range 2497 2501; range 2509 2510; range 2530 2532;
    range 2561 2563; range 2620 2621; range 2625 2627;
    range 2631 2633; range 2635 2638; range 2672 2674;
    range 2689 2691; range 2748 2749; range 2753 2758;
    range 2759 2761; range 2765 2766; range 2786 2788;
    range 2817 2818; range 2876 2877; range 2879 2880;
    range 2881 2884; range 2893 2894; range 2902 2903;
    range 2946 2947; range 3008 3009; range 3021 3022;
    range 3134 3137; range 3142 3145; range 3146 3150;
    range 3157 3159; range 3260 3261; range 3263 3264;
    range 3270 3271; range 3276 3278; range 3393 3396;
    range 3405 3406; range 3530 3531; range 3538 3541;
    range 3542 3543; range 3633 3634; range 3636 3643;
    range 3655 3663; range 3761 3762; range 3764 3770;
    range 3771 3773; range 3784 3790; range 3864 3866;
    range 3893 3894; range 3895 3896; range 3897 3898;
    range 3953 3967; range 3968 3973; range 3974 3976;
    range 3984 3992; range 3993 4029; range 4038 4039;
    range 4141 4145; range 4146 4147; range 4150 4152;
    range 4153 4154; range 4184 4186; range 5906 5909;
    range 5938 5941; range 5970 5972; range 6002 6004;
    range 6071 6078; range 6086 6087; range 6089 6100;
    range 6109 6110; range 6155 6158; range 6313 6314;
    range 6432 6435; range 6439 6441; range 6450 6451;
    range 6457 6460; range 8400 8413; range 8417 8418;
    range 8421 8427; range 12330 12336; range 12441 12443;
    range 64286 64287; range 65056 65060;
  ]) categories in
  let categories = SMap.add "Me" (RangeSet.make [
    range 1160 1162; range 1758 1759; range 8413 8417;
    range 8418 8421;
  ]) categories in
  let categories = SMap.add "Mc" (RangeSet.make [
    range 2307 2308; range 2366 2369; range 2377 2381;
    range 2434 2436; range 2494 2497; range 2503 2505;
    range 2507 2509; range 2519 2520; range 2563 2564;
    range 2622 2625; range 2691 2692; range 2750 2753;
    range 2761 2762; range 2763 2765; range 2818 2820;
    range 2878 2879; range 2880 2881; range 2887 2889;
    range 2891 2893; range 2903 2904; range 3006 3008;
    range 3009 3011; range 3014 3017; range 3018 3021;
    range 3031 3032; range 3073 3076; range 3137 3141;
    range 3202 3204; range 3262 3263; range 3264 3269;
    range 3271 3273; range 3274 3276; range 3285 3287;
    range 3330 3332; range 3390 3393; range 3398 3401;
    range 3402 3405; range 3415 3416; range 3458 3460;
    range 3535 3538; range 3544 3552; range 3570 3572;
    range 3902 3904; range 3967 3968; range 4140 4141;
    range 4145 4146; range 4152 4153; range 4182 4184;
    range 6070 6071; range 6078 6086; range 6087 6089;
    range 6435 6439; range 6441 6444; range 6448 6450;
    range 6451 6457;
  ]) categories in
  let categories = SMap.add "Nl" (RangeSet.make [
    range 5870 5873; range 8544 8580; range 12295 12296;
    range 12321 12330; range 12344 12347;
  ]) categories in
  let categories = SMap.add "Zl" (RangeSet.make [
    range 8232 8233;
  ]) categories in
  let categories = SMap.add "Zp" (RangeSet.make [
    range 8233 8234;
  ]) categories in
  let categories = SMap.add "Cs" (RangeSet.make [
    range 55296 55297; range 56191 56193; range 56319 56321;
    range 57343 57344;
  ]) categories in
  let categories = SMap.add "Co" (RangeSet.make [
    range 57344 57345; range 63743 63744;
  ]) categories in
  let union subs = List.fold_left
    (fun ranges category ->
      RangeSet.union ranges (SMap.find category categories))
    RangeSet.empty subs in
  let categories =SMap.add
    "C" (union ["Cc"; "Cf"; "Co"; "Cs"]) categories in
  let categories =SMap.add
    "L" (union ["Ll"; "Lm"; "Lo"; "Lt"; "Lu"]) categories in
  let categories =SMap.add
    "M" (union ["Mc"; "Me"; "Mn"]) categories in
  let categories =SMap.add
    "N" (union ["Nd"; "Nl"; "No"]) categories in
  let categories =SMap.add
    "P" (union ["Pc"; "Pd"; "Pe"; "Pf"; "Pi"; "Po"; "Ps"]) categories in
  let categories =SMap.add
    "S" (union ["Sc"; "Sk"; "Sm"; "So"]) categories in
  let categories =SMap.add
    "Z" (union ["Zl"; "Zp"; "Zs"]) categories in
  categories)

let get category_name =
  if SMap.mem category_name category_map then
    Some (SMap.find category_name category_map)
  else
    None

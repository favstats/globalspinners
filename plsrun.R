
library(tidyverse)
## install this if you dont have it
# remotes::install_github("favstats/metatargetr")
library(metatargetr)

remainers <- c("101202171838362", "290631594122768", "183550948164225", "314499212242", 
               "2353537341582545", "234603473062976", "83439828234", "103805636333615", 
               "168661083194412", "250316728161309", "924215500923867", "182393114948781", 
               "104491695993540", "260910123767099", "848008481893903", "190936465037027", 
               "104773258848361", "253068281211731", "102520058606826", "221695101029367", 
               "208263699040667", "107217383988190", "842788945820369", "109918909035681", 
               "111357474528605", "141627992572943", "1488853891429349", "289446011076088", 
               "316256535073131", "168743617253", "272964606172562", "332455686940366", 
               "137877409609952", "63563473556", "102931488739", "228168274881", 
               "1539769832715676", "339855706171439", "149388985089327", "57544755889", 
               "157458232131", "1971966879776579", "618179678235315", "273238932923", 
               "209987935541855", "634178786679448", "135010893177571", "114988538373074", 
               "135889759813803", "65335771090", "106215895116028", "106058134201158", 
               "513969188677442", "223618785182273", "547821942058909", "228700014146287", 
               "107475815326689", "114606549484", "8304333127", "197825446750111", 
               "104288362419972", "1395939193764858", "111024331618659", "230785173665770", 
               "427656597297153", "523106417867452", "1723480164579572", "1133289623473565", 
               "1816310825284614", "153217548705577", "262881224074686", "174291436685142", 
               "1449284915381234", "101764188259244", "106074795911048", "100580925140105", 
               "102601499822802", "471764082950159", "291177524360702", "103932459326736", 
               "182546041796641", "528408150903547", "103445844473825", "1577877239102295", 
               "147115405150941", "141108216058737", "695492003874695", "127610057890", 
               "130929705668", "238683972654117", "346625951629", "174545086044880", 
               "221798457888029", "151159271594797", "608975645850217", "1737852076287450", 
               "8040892957", "122854408319", "643137272398136", "121439954563203", 
               "132715103269897", "351616078284404", "192856493908290", "1400615623488784", 
               "574079826086251", "109545244848137", "117221101343678", "106461617679070", 
               "107026039151450", "180161138515248", "150106531736299", "100848425054921", 
               "102161401415097", "192136870799912", "1987488401557504", "126436680719285", 
               "218904931482352", "477935665706418", "102251656262079", "1471581409720618", 
               "102731029554453", "323383091040924", "1563310070576778", "109470364774303", 
               "287829634407040", "179670408573941", "109937650579639", "137725446093407", 
               "247944825069932", "106939425841911", "206212622563937", "264498770075583", 
               "113730508499849", "775974615850063", "192548643939288", "220803731286272", 
               "294493857651676", "179497582061065", "102845669064210", "490930117644709", 
               "648648261893473", "109323929038", "120343698070515", "148475335184300", 
               "238507146264909", "105556085524776", "162989077434855", "78617029137", 
               "231350380299380", "117702158266385")


page_info_dat3 <- remainers %>% 
  map_dfr(~{
    get_page_insights2(pageid=.x,
                       # lang="en-GB", iso2c="US", 
                       include_info=c("page_info"))
  })


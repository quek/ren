-record(rensa_word, {
          name,
          f=nil,
          code=nil,
          hidden=false,
          immed=false
         }).

-record(rensa_context, {
          s=[],
          r=[],
          cp,
          compile=false,
          here,
          latest,
          d,
          buffer=""
         }).

-record(word, {
          name,
          f=nil,
          code=nil,
          hidden=false,
          immed=false
         }).

-record(context, {
          s=[],
          r=[],
          cp,
          compile=false,
          here,
          latest,
          source={standard_io, [], 0},
          debug=0
         }).

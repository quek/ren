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
          d,
          buffer="",
          source=standard_io
         }).

-record(src, {
          in=standard_io,
          buffer=[],
          line=0,
          use=[core, biw],
          module=scratch
         }).

-record(context, {
          s=[],
          r=[],
          cp,
          compile=false,
          here=[],
          latest,
          source=#src{},
          debug=0
         }).

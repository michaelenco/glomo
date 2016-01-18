{application,p1_tls,
             [{description,"OpenSSL wrapper"},
              {vsn,"0.2.0"},
              {modules,[p1_sha,p1_tls,p1_tls_app,p1_tls_sup]},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{p1_tls_app,[]}}]}.

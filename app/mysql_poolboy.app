{application,mysql_poolboy,
             [{registered,[]},
              {description,"MySQL/OTP + Poolboy"},
              {vsn,"0.2.1"},
              {modules,[mysql_poolboy,mysql_poolboy_app,mysql_poolboy_sup]},
              {mod,{mysql_poolboy_app,[]}},
              {applications,[kernel,stdlib,mysql,poolboy]}]}.

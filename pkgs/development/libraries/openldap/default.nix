{stdenv, fetchurl, openssl, cyrus_sasl, db, groff}:

stdenv.mkDerivation rec {
  name = "openldap-2.4.40";

  src = fetchurl {
    url = "http://www.openldap.org/software/download/OpenLDAP/openldap-release/${name}.tgz";
    sha256 = "1nyslrgwxwilgv5sixc37svls5rbvhsv9drb7hlrjr2vqaji29ni";
  };

  buildInputs = [ openssl cyrus_sasl db groff ];

  # http://www.openldap.org/its/index.cgi/Build?id=8056;page=1
  patches = [ ./gcc-5.patch ];

  configureFlags =
    [ "--enable-overlays"
      "--disable-dependency-tracking"   # speeds up one-time build
    ] ++ stdenv.lib.optional (openssl == null) "--without-tls"
      ++ stdenv.lib.optional (cyrus_sasl == null) "--without-cyrus-sasl";

  dontPatchELF = 1; # !!!

  # Fixup broken libtool
  preFixup = ''
    sed -e 's,-lsasl2,-L${cyrus_sasl}/lib -lsasl2,' \
        -e 's,-lssl,-L${openssl}/lib -lssl,' \
        -i $out/lib/libldap.la -i $out/lib/libldap_r.la
  '';

  meta = with stdenv.lib; {
    homepage    = http://www.openldap.org/;
    description = "An open source implementation of the Lightweight Directory Access Protocol";
    maintainers = with maintainers; [ lovek323 mornfall ];
    platforms   = platforms.unix;
  };
}

# This file has been generated by node2nix 1.11.1. Do not edit!
{
  nodeEnv,
  fetchurl,
  nix-gitignore,
  stdenv,
  lib,
  globalBuildInputs ? [],
}: let
  sources = {
    "@aws-crypto/crc32-5.2.0" = {
      name = "_at_aws-crypto_slash_crc32";
      packageName = "@aws-crypto/crc32";
      version = "5.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@aws-crypto/crc32/-/crc32-5.2.0.tgz";
        sha512 = "nLbCWqQNgUiwwtFsen1AdzAtvuLRsQS8rYgMuxCrdKf9kOssamGLuPwyTY9wyYblNr9+1XM8v6zoDTPPSIeANg==";
      };
    };
    "@aws-crypto/util-5.2.0" = {
      name = "_at_aws-crypto_slash_util";
      packageName = "@aws-crypto/util";
      version = "5.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@aws-crypto/util/-/util-5.2.0.tgz";
        sha512 = "4RkU9EsI6ZpBve5fseQlGNUWKMa1RLPQ1dnjnQoe07ldfIzcsGb5hC5W0Dm7u423KWzawlrpbjXBrXCEv9zazQ==";
      };
    };
    "@aws-sdk/types-3.696.0" = {
      name = "_at_aws-sdk_slash_types";
      packageName = "@aws-sdk/types";
      version = "3.696.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@aws-sdk/types/-/types-3.696.0.tgz";
        sha512 = "9rTvUJIAj5d3//U5FDPWGJ1nFJLuWb30vugGOrWk7aNZ6y9tuA3PI7Cc9dP8WEXKVyK1vuuk8rSFP2iqXnlgrw==";
      };
    };
    "@mlabs-haskell/cardano-data-lite-git+ssh://git@github.com/mlabs-haskell/cardano-data-lite.git#0fd1d12dc3c1e29f98c4d68592e6d6014ef721aa" = {
      name = "_at_mlabs-haskell_slash_cardano-data-lite";
      packageName = "@mlabs-haskell/cardano-data-lite";
      version = "1.0.0";
      src = builtins.fetchGit {
        url = "ssh://git@github.com/mlabs-haskell/cardano-data-lite.git";
        rev = "0fd1d12dc3c1e29f98c4d68592e6d6014ef721aa";
      };
    };
    "@mlabs-haskell/json-bigint-2.0.0" = {
      name = "_at_mlabs-haskell_slash_json-bigint";
      packageName = "@mlabs-haskell/json-bigint";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@mlabs-haskell/json-bigint/-/json-bigint-2.0.0.tgz";
        sha512 = "JX9TON+nZbt+1TJ5MNV1Gcpxp3/m56x1/glDwzGtydrzQzyZbKg4XFw9Frib6fh89YVqjSFJ9xmVeIyDJ5DxTQ==";
      };
    };
    "@noble/ed25519-2.1.0" = {
      name = "_at_noble_slash_ed25519";
      packageName = "@noble/ed25519";
      version = "2.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@noble/ed25519/-/ed25519-2.1.0.tgz";
        sha512 = "KM4qTyXPinyCgMzeYJH/UudpdL+paJXtY3CHtHYZQtBkS8MZoPr4rOikZllIutJe0d06QDQKisyn02gxZ8TcQA==";
      };
    };
    "@noble/hashes-1.6.1" = {
      name = "_at_noble_slash_hashes";
      packageName = "@noble/hashes";
      version = "1.6.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@noble/hashes/-/hashes-1.6.1.tgz";
        sha512 = "pq5D8h10hHBjyqX+cfBm0i8JUXJ0UhczFc4r74zbuT9XgewFo2E3J1cOaGtdZynILNmQ685YWGzGE1Zv6io50w==";
      };
    };
    "@smithy/is-array-buffer-2.2.0" = {
      name = "_at_smithy_slash_is-array-buffer";
      packageName = "@smithy/is-array-buffer";
      version = "2.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@smithy/is-array-buffer/-/is-array-buffer-2.2.0.tgz";
        sha512 = "GGP3O9QFD24uGeAXYUjwSTXARoqpZykHadOmA8G5vfJPK0/DC67qa//0qvqrJzL1xc8WQWX7/yc7fwudjPHPhA==";
      };
    };
    "@smithy/types-3.7.2" = {
      name = "_at_smithy_slash_types";
      packageName = "@smithy/types";
      version = "3.7.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@smithy/types/-/types-3.7.2.tgz";
        sha512 = "bNwBYYmN8Eh9RyjS1p2gW6MIhSO2rl7X9QeLM8iTdcGRP+eDiIWDt66c9IysCc22gefKszZv+ubV9qZc7hdESg==";
      };
    };
    "@smithy/util-buffer-from-2.2.0" = {
      name = "_at_smithy_slash_util-buffer-from";
      packageName = "@smithy/util-buffer-from";
      version = "2.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@smithy/util-buffer-from/-/util-buffer-from-2.2.0.tgz";
        sha512 = "IJdWBbTcMQ6DA0gdNhh/BwrLkDR+ADW5Kr1aZmd4k3DIF6ezMV4R2NIAmT08wQJ3yUK82thHWmC/TnK/wpMMIA==";
      };
    };
    "@smithy/util-utf8-2.3.0" = {
      name = "_at_smithy_slash_util-utf8";
      packageName = "@smithy/util-utf8";
      version = "2.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@smithy/util-utf8/-/util-utf8-2.3.0.tgz";
        sha512 = "R8Rdn8Hy72KKcebgLiv8jQcQkXoLMOGGv5uI1/k0l+snqkOzQ1R0ChUBCxWMlBsFMekWjq0wRudIweFs7sKT5A==";
      };
    };
    "base58-js-2.0.0" = {
      name = "base58-js";
      packageName = "base58-js";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/base58-js/-/base58-js-2.0.0.tgz";
        sha512 = "nAV5d32QXuGcGptSApkKpC1gGakWBnfJMNjKrYTBh4tb0szfZF+ooueFLy8T4VrY+o4SrE/TyrtUnRZcwZchaA==";
      };
    };
    "bech32-2.0.0" = {
      name = "bech32";
      packageName = "bech32";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/bech32/-/bech32-2.0.0.tgz";
        sha512 = "LcknSilhIGatDAsY1ak2I8VtGaHNhgMSYVxFrGLXv+xLHytaKZKcaUJJUE7qmBr7h33o5YQwP55pMI0xmkpJwg==";
      };
    };
    "tslib-2.8.1" = {
      name = "tslib";
      packageName = "tslib";
      version = "2.8.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/tslib/-/tslib-2.8.1.tgz";
        sha512 = "oJFu94HQb+KVduSUQL7wnpmqnfmLsOA/nAh6b6EH0wCEoK0/mPeXU6c3wKDV83MkOuHPRHtSXKKU99IBazS/2w==";
      };
    };
    "tweetnacl-1.0.3" = {
      name = "tweetnacl";
      packageName = "tweetnacl";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/tweetnacl/-/tweetnacl-1.0.3.tgz";
        sha512 = "6rt+RN7aOi1nGMyC4Xa5DdYiukl2UWCbcJft7YhxReBGQD7OAM8Pbxw6YMo4r2diNEA8FEmu32YOn9rhaiE5yw==";
      };
    };
  };
  args = {
    name = "purescript-cardano-types";
    packageName = "purescript-cardano-types";
    src = ./..;
    dependencies = [
      sources."@aws-crypto/crc32-5.2.0"
      sources."@aws-crypto/util-5.2.0"
      sources."@aws-sdk/types-3.696.0"
      sources."@mlabs-haskell/cardano-data-lite-git+ssh://git@github.com/mlabs-haskell/cardano-data-lite.git#0fd1d12dc3c1e29f98c4d68592e6d6014ef721aa"
      sources."@mlabs-haskell/json-bigint-2.0.0"
      sources."@noble/ed25519-2.1.0"
      sources."@noble/hashes-1.6.1"
      sources."@smithy/is-array-buffer-2.2.0"
      sources."@smithy/types-3.7.2"
      sources."@smithy/util-buffer-from-2.2.0"
      sources."@smithy/util-utf8-2.3.0"
      sources."base58-js-2.0.0"
      sources."bech32-2.0.0"
      sources."tslib-2.8.1"
      sources."tweetnacl-1.0.3"
    ];
    buildInputs = globalBuildInputs;
    meta = {
    };
    production = false;
    bypassCache = true;
    reconstructLock = false;
  };
in {
  args = args;
  sources = sources;
  tarball = nodeEnv.buildNodeSourceDist args;
  package = nodeEnv.buildNodePackage args;
  shell = nodeEnv.buildNodeShell args;
  nodeDependencies = nodeEnv.buildNodeDependencies (lib.overrideExisting args {
    src = stdenv.mkDerivation {
      name = args.name + "-package-json";
      src =
        nix-gitignore.gitignoreSourcePure [
          "*"
          "!package.json"
          "!package-lock.json"
        ]
        args.src;
      dontBuild = true;
      installPhase = "mkdir -p $out; cp -r ./* $out;";
    };
  });
}
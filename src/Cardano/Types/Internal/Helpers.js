import { bech32 } from "bech32";

export const _decodeUtf8 = buffer => left => right => {
  let decoder = new TextDecoder("utf-8", { fatal: true }); // Without fatal=true it never fails

  try {
    return right(decoder.decode(buffer));
  } catch (err) {
    return left(err);
  }
};

export const _clone = x => {
  if (
    typeof x.to_bytes === "function" &&
    typeof x.constructor.from_bytes === "function"
  ) {
    return x.constructor.from_bytes(x.to_bytes());
  } else {
    return x;
  }
};

export const encodeBech32 = prefix => bytes => {
  return bech32.encode(prefix, bech32.toWords(bytes), Number.MAX_SAFE_INTEGER);
};

export const _decodeBech32 = either => str => {
  const res = bech32.decode(str);
  if (typeof res == "string") {
    return either.left(res);
  } else {
    const bytes = new Uint8Array(bech32.fromWords(res.words));
    return either.right({ prefix: res.prefix, bytes });
  }
};

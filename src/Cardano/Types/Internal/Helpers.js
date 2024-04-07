export const _decodeUtf8 = buffer => left => right => {
  let decoder = new TextDecoder("utf-8", { fatal: true }); // Without fatal=true it never fails

  try {
    return right(decoder.decode(buffer));
  } catch (err) {
    return left(err);
  }
};

export const _clone = x => {
  if (typeof x.to_bytes === 'function' &&
      typeof x.constructor.from_bytes === 'function') {
    return x.constructor.from_bytes(x.to_bytes());
  } else {
    return x;
  }
};

# Netlify Edge Functions

There are several quirks with running [Netlify Edge Functions](https://docs.netlify.com/edge-functions/overview/) that are important
to know when developing:

- Any file on the root of `netlify/edge-functions` is considered a
  module that exposes an edge function.
- Files in folders within `netlify/edge-functions` will be skipped as edge
  functions, but the folder itself, can't be named the same as an edge
  function—doing so will result in broken path matching.

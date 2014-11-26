
BEGIN { RS = "[^a-z0-9]+" }
// { words[$0]++ }
END {
  for (w in words)
    print words[w] " : " w
    }

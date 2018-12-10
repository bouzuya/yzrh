# yzrh

template

## How to run

```
$ npm --silent run build
$ npm --silent start -- --in-file config/routes.rb --in-format routes.rb | jq --sort-keys .
```

## Note

### Command-line option

- `--flag` ... long name `flag` boolean option
  - `--flag=true` ... long name `flag` value `true` *string* option
- `--name john` ... long name `name` value `john` string option
- `--name=john` ... long name `name` value `john` string option
- `-f` ... short name `f` boolean option
- `-n john` ... short name `n` value `john` string option
- `-n=john` ... short name `n` value `john` string option
- `-fg` ... short name `f` boolean option & short name `g` boolean option
  - `-ofile` ... short name `o`, `f`, `i`, `l`, `e` boolean options (imcompatible with optparse-applicative)

## License

[MIT](LICENSE)

## Author

[bouzuya][user] &lt;[m@bouzuya.net][email]&gt; ([https://bouzuya.net/][url])

[user]: https://github.com/bouzuya
[email]: mailto:m@bouzuya.net
[url]: https://bouzuya.net/

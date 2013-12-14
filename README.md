fortran-construct-complete
==========================

A Vim ftplugin for fortran to complete fortran comstruct automaticly.

Examples

```
real function add(a, b) results(s)<Enter>
```
to
```
real function add(a, b) results(s)
    |
end function
```

```
if a > 3<Enter>
```
to
```
if (a > 3) then
    |
endif
```

```
do i=1,n<Enter>
```
to
```
do i = 1, n
    |
enddo
```

When press <Enter>, this plugin try to parse the current line, if valid, 
complete the current line. For example
```
if a 3<Enter>
```
shall not add a =endif= since the current line is not valid.

I try to make it to be accurate, so I used pyparsing to parse the current line, not just some regular expression. So <Enter> is can be rather safely binded.

+python3 is needed since pyparsing is used.


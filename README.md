# Cryptographer: Convenient Encryption #

Cryptographer is a tool designed to encrypt text in such a way that it is easily accessible from many devices without requiring anything but a web browser. To achieve this, it generates an html file with the decryption algorithm included as inline JavaScript and provides an input field for the key.

Currently, it uses the [Twofish](https://www.schneier.com/twofish.html) cipher which is secure, fast and free. The mode of operation used is CBC.

## Setup

The program can be installed using Cabal, however it requires the [Kwargs](https://github.com/netogallo/kwargs) package which is not yet available in hackage. The simple steps are:

0) Install Haskell, cabal and cabal-install. The easiest way is through the [Haskell Platform](https://www.haskell.org/platform/).

1) Install Kwargs:

```
  > git clone https://github.com/netogallo/Kwargs.git
  > cd Kwargs
  > cabal install Kwargs
```

2) Install Cryptographer:

``` 
  > git clone https://github.com/netogallo/Cryptographer.git
  > cd Cryptographer
  > cabal install Cryptographer
```

This generates an executable called *cryptographer* inside your cabal bin directory.

## Usage

Currently, there is very limited functionality limited to taking data on the standard input, encrypting it with the provided key and writing the generated html to the standard output. For example:

```
> echo "Deepest darkest secrets ever" | cryptographer --key "Long and Strong" > secrets.html
```

When *secrets* is opened in a web browser, it contains an input field that provided with the key, will show the encrypted data. Note that some HTML previewers (like the one on GMail) don't execute JavaScript for security reasons so make sure you download the file and open it with a web browser. TODO: Add a warning in the generated HTMLs :P

ALWAYS REMEMBER TO USE SECURE PASSWORDS. Even though there are no known security vulnerabilities in TwoFish (There are some hypothesis about possible attacks which are slightly more efficient than exhaustive search but nothing certain atm. See [Rumors](https://www.schneier.com/blog/archives/2005/11/twofish_cryptan.html)). The sha-256 hash is used for keys and with enough expensive hardware (for mining Bitcoins...), an exhaustive key search is feasible for weak passwords.
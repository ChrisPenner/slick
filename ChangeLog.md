# Changelog for slick

## 1.1.2.2
- Add missing export for loadUsingMeta

## 1.1.2.1
- Add missing export for orgModeToHTML'

## 1.1.2.0
- Upgrade to lts-18.9 and add support for org-mode documents.

## 1.1.1.1
- Upgrade to lts-17.8 and ghc-8.10.4

## 1.1.1.0
- Add `loadUsingMeta` to allow specifying a separate pandoc meta writer.

## 1.1.0.0
- Upgrade to lts-16.29

## 1.0.1.0
- Add `makePandocReaderWithMetaWriter` and `makePandocReaderWithMetaWriter'` for reading in resources with custom Pandoc Writers for metadata.

## 1.0.0.0
- Deprecate Slick.Caching, Simplify all other exports down.
- Switch to recommending `Shake.Development.Forward`

## 0.2.0.0
- Allow IO in `makePandocReader` and `makePandocWriter` to allow use of complex filters, etc.

## 0.1.1.0
- Add gfm markdown options on writer extensions
    - this should allow pandoc to auto-generate id's on header tags

## 0.1.0.1
- Docs update

## 0.1.0.0
- Initial release

## Unreleased changes

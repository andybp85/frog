import fs from "fs"
import path from "path"
import uncss from "uncss"
import postcss from "postcss"
import autoprefixer from "autoprefixer"
import cssnano from "cssnano"
import stripCSSComments from "strip-css-comments"
import htmlmin from "html-minifier"

async function main(inFileName) {
    if (inFileName === undefined) throw Error("No input file")

    const read = await new Promise((resolve, reject) =>
        fs.readFile(inFileName, "utf8", async (err, html) =>
            err
                ? reject(err)
                : resolve({
                    html,
                    css: await new Promise(resolveUncss =>
                        uncss([inFileName],
                            {
                                csspath: "css",
                                htmlroot: "."
                            },
                            (err, css) =>
                            err
                                ? reject(err)
                                : resolveUncss(css)))
                })))

    const pcss = await postcss([autoprefixer, cssnano])
        .process(
            stripCSSComments(read.css, {preserve: false}),
            {from: undefined})

    const minified = htmlmin.minify(
        read.html
            .replace(/<link rel="stylesheet".*?>/g, "")
            .replace(/<!-- CSS -->/, `<style>${pcss.css}</style>`),
        {
            collapseWhitespace: true,
            removeComments: true,
            minifyJS: true
        })

    fs.writeFileSync(inFileName, minified, "utf8")
}

main(process.argv[2])
    .then(() => process.exit(0))
    .catch(err => console.log(err) || process.exit(1))

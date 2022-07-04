import https from 'https'

// :: (string) -> Promise(true, string)
const ensureUrlReached = (url) => {
    return new Promise((resolve, reject) => {
        console.error(`Checking ${url}`)
        https.get(url, (res) => {
            if (res.statusCode === 200 || res.statusCode === 301) {
                resolve(true)
            } else {
                reject(`Error: ${url} status ${res.statusCode}`)
            }
        })
    })
}

export {
    ensureUrlReached,
}

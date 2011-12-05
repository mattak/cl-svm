(ns ma3.net
	(:use ma3.io)
	(:import
		(org.apache.http HttpHost)
		(org.apache.http.client HttpClient)
		(org.apache.http.client.methods HttpGet)
		(org.apache.http.impl.client DefaultHttpClient)
		(org.apache.http.conn.params ConnRoutePNames))
)

;; etc
;;---------------
(defn env_proxy_and_port []
	(let [orig (System/getenv "http_proxy")]
		(if (nil? orig)
				nil
			(let [host_port (apply str (filter #(not (= %1 \/)) (nth (.split orig "\\/\\/") 1)))]
				(let [host_port (.split host_port ":")]
					[(nth host_port 0) (Integer/parseInt (nth host_port 1))]
					)))))


(defn httpget [url & [{:keys [host port save]}]]
	(let [client (DefaultHttpClient.)
				method (HttpGet. url)
				proxy (if (and host port)
									(HttpHost. host port)
									nil)]
		(if proxy
			(doto (.getParams client)
				(.setParameter ConnRoutePNames/DEFAULT_PROXY proxy)))
		(if-let [response
						(try (. client execute method)
							(catch Exception e (.println *err* "Error") nil))]
			(if save
				(is2file (.. response getEntity getContent) save)
				(is2str (.. response getEntity getContent))
				))))

(defn httpget-autoproxy [url & [{:keys [save]}]]
	(if-let [prx (env_proxy_and_port)]
			(if save
					(httpget url {:host (first prx) :port (second prx) :save save})
					(httpget url {:host (first prx) :port (second prx)}))
			(if save
					(httpget url {:save save})
					(httpget url))))


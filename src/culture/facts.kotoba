(ns culture.facts
  "Country-level regional-culture catalog for Bahrain (BHR) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"BHR"
   [{:culture/id "bhr.dish.machboos"
     :culture/name "Machboos"
     :culture/name-local "مچبوس"
     :culture/country "BHR"
     :culture/kind :dish
     :culture/summary "Arab mixed rice dish, also known as kabsa or makboos, originating from Saudi Arabia or Yemen; commonly regarded as a national dish in all the countries of the Gulf Cooperation Council."
     :culture/url "https://en.wikipedia.org/wiki/Kabsa"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.dish.muhammar"
     :culture/name "Muhammar"
     :culture/name-local "محمر"
     :culture/country "BHR"
     :culture/kind :dish
     :culture/summary "Traditional dish from Bahrain: sweet rice seasoned with spices and date molasses, usually eaten with fried or grilled fish."
     :culture/url "https://en.wikipedia.org/wiki/Muhammar"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.dish.harees"
     :culture/name "Harees"
     :culture/name-local "هريس"
     :culture/country "BHR"
     :culture/kind :dish
     :culture/summary "Porridge of cracked wheat mixed with meat, spices and butter, eaten in Bahrain during Ramadan and Ashura; inscribed as UNESCO Intangible Cultural Heritage in 2023 by Bahrain and six other states."
     :culture/url "https://en.wikipedia.org/wiki/Harees"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.dish.balaleet"
     :culture/name "Balaleet"
     :culture/name-local "بلاليط"
     :culture/country "BHR"
     :culture/kind :dish
     :culture/summary "Traditional sweet and savoury dish of Eastern Arabian cuisine originating in the United Arab Emirates, Bahrain and Qatar: sweetened vermicelli served with an overlying egg omelette, popular at breakfast and during Eid al-Fitr."
     :culture/url "https://en.wikipedia.org/wiki/Balaleet"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.beverage.arabic-coffee"
     :culture/name "Arabic coffee"
     :culture/name-local "قهوة"
     :culture/country "BHR"
     :culture/kind :beverage
     :culture/summary "Brewed coffee central to Middle Eastern hospitality; Gulf-style qahwa khaleeji is coloured bright yellow by saffron and cardamom, and Arabic coffee is a UNESCO-confirmed Intangible Cultural Heritage of Arab states."
     :culture/url "https://en.wikipedia.org/wiki/Arabic_coffee"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.product.halwa-showaiter"
     :culture/name "Halwa Showaiter"
     :culture/name-local "حلوى شويطر"
     :culture/country "BHR"
     :culture/kind :product
     :culture/summary "Sweet popular in the Persian Gulf region, invented by the Showaiter family based in Muharraq, Bahrain."
     :culture/url "https://en.wikipedia.org/wiki/Showaiter"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.heritage.qalat-al-bahrain"
     :culture/name "Qal'at al-Bahrain"
     :culture/name-local "قلعة البحرين"
     :culture/country "BHR"
     :culture/kind :heritage
     :culture/summary "Archaeological site near Manama, once the capital of the Dilmun civilization and the oldest fortification in the Persian Gulf region; inscribed as a UNESCO World Heritage Site in 2005."
     :culture/url "https://en.wikipedia.org/wiki/Qal%27at_al-Bahrain"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "bhr.heritage.bahrain-pearling-trail"
     :culture/name "Bahrain Pearling Trail"
     :culture/country "BHR"
     :culture/kind :heritage
     :culture/summary "Serial site on Muharraq Island of oyster beds, historic buildings and a 3.5 km pathway testifying to Bahrain's pearl-diving economy; inscribed in 2012 as Bahrain's second UNESCO World Heritage Site."
     :culture/url "https://en.wikipedia.org/wiki/Bahrain_Pearling_Trail"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-bhr culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "BHR"))
                 " BHR entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))

#### GDAX Public Client API functions ####
# The following packages are required: httr, jsonlite.
# All functions accept an optional proxy argument to use when
# connecting to GDAX (httr::use_proxy() is recommended).

# GDAX API base URL
url <- "https://api.gdax.com"

#### API Functions ####
getProducts <- function(proxy = NULL) {
  # Gets information about the currency pairs available for trading.
  #
  # Returns:
  #   A data frame with columns:
  #     id (character):
  #       Currency pair identifier, e.g. "BTC-USD"
  #     base_currency (character):
  #       Base currency identifier, e.g. "BTC"
  #     quote_currency (character):
  #       Quote currency identifier, e.g. "USD"
  #     base_min_size (numeric):
  #       Base currency minimum order size
  #     base_max_size (numeric):
  #       Base currency maximum order size
  #     quote_increment (numeric):
  #       Quote currency minimum price and increment
  #     display_name (character):
  #       Standard format for the currency pair, e.g. "BTC/USD"
  #     margin_enabled (logical):
  #       Availability of margin trading for the currency pair
  
  # Path
  path <- "/products"

  # Modify url
  url <- httr::modify_url(url, path = path)

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    message <- content$message
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))

  # Change non-character types
  content$base_min_size <- as.numeric(content$base_min_size)
  content$base_max_size <- as.numeric(content$base_max_size)
  content$quote_increment <- as.numeric(content$quote_increment)
  content$margin_enabled <- as.logical(content$margin_enabled)

  # Return content
  return(as.data.frame(content))
}

getProductOrderBook <- function(product_id, level = 1, proxy = NULL) {
  # Gets the open bids and asks of a product. The amount of detail can be 
  # specified by the level parameter:
  #   Level 1: Inside (best) bid and ask (aggregated)
  #   Level 2: Top 50 bids and asks (aggregated)
  #   Level 3: Full order book (non aggregated)
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  #   level (numeric):
  #     Level of order book detail, {1,2,3}
  #
  # Returns:
  #   A list containing:
  #     sequence (numeric):
  #       Order book request sequence number
  #     asks (data frame):
  #       price (numeric):
  #         Quoted ask price
  #       size (numeric):
  #         Available amount for the quoted ask price
  #       num_orders (numeric):
  #         Aggregated number of asks (levels 1 and 2)
  #       id (character):
  #         Ask identifier (level 3)
  #     bids (data frame):
  #       price (numeric):
  #         Quoted bid price
  #       size (numeric):
  #         Available amount for the quoted bid price
  #       num_orders (numeric):
  #         Aggregated number of bids (levels 1 and 2)
  #       id (character):
  #         Bid identifier (level 3)
  
  # Path
  path <- paste("/products/", product_id, "/book", sep = "")

  # Modify url
  url <- httr::modify_url(url, path = path, query = list("level" = level))

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    message <- content$message
    stop(message)
  }
  
  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))
  
  # Change non-character types
  if ((level == 1) || (level == 2)) {
    class(content$bids) <- "numeric"
    class(content$asks) <- "numeric"
    content$bids <- as.data.frame(content$bids)
    content$asks <- as.data.frame(content$asks)
    names(content$bids) <- c("price", "size", "num_orders")
    names(content$asks) <- c("price", "size", "num_orders")
  } else {
    if (level == 3) {
      content$bids <- as.data.frame(content$bids)
      content$asks <- as.data.frame(content$asks)
      content$bids[,c(1,2)] <- apply(content$bids[,c(1,2)], 2, as.numeric)
      content$asks[,c(1,2)] <- apply(content$asks[,c(1,2)], 2, as.numeric)
      names(content$bids) <- c("price", "size", "id")
      names(content$asks) <- c("price", "size", "id")
    }
  }

  # Return content
  return(content)
}

getProductTicker <- function(product_id, proxy = NULL) {
  # Gets information about the last trade (tick), best bid/ask prices and 
  # 24 h volume for a product.
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  #
  # Returns:
  #   A data frame with columns:
  #     trade_id (numeric):
  #       Identifier of the last trade
  #     price (numeric):
  #       Last trade price in quote currency units
  #     size (numeric):
  #       Last trade size in base currency units
  #     bid (numeric):
  #       Quoted bid price (best) at time of last trade
  #     ask (numeric):
  #       Quoted ask price (best) at time of last trade
  #     volume (numeric):
  #       Trading volume during last 24 h timeslice
  #     time (POSIXt):
  #       Time of last trade
  
  # Path
  path <- paste("/products/", product_id, "/ticker", sep = "")

  # Modify url
  url <- httr::modify_url(url, path = path)

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    message <- content$message
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))

  # Change non-character types
  content$price <- as.numeric(content$price)
  content$size <- as.numeric(content$size)
  content$bid <- as.numeric(content$bid)
  content$ask <- as.numeric(content$ask)
  content$volume <- as.numeric(content$volume)
  content$time <- strptime(content$time, "%Y-%m-%dT%H:%M:%OS")

  # Return content
  return(as.data.frame(content))
}

getProductTrades <- function(product_id, proxy = NULL) {
  # Gets the last 100 trades for a product.
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  #
  # Returns:
  #   A data frame with columns:
  #     time (POSIXt):
  #       Time of trade
  #     trade_id (numeric):
  #       Identifier of the trade
  #     price (numeric):
  #       Trade price in quote currency units
  #     size (numeric):
  #       Trade size in base currency units
  #     side (character):
  #       Maker order side, {"buy", "sell"}
  
  # Path
  path <- paste("/products/", product_id, "/trades", sep = "")

  # Modify url
  url <- httr::modify_url(url, path = path)

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    message <- content$message
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))
  content <- as.data.frame(content[nrow(content):1,])
  rownames(content) <- rev(rownames(content))

  # Change non-character types
  content$time <- strptime(content$time, "%Y-%m-%dT%H:%M:%OS")
  content$price <- as.numeric(content$price)
  content$size <- as.numeric(content$size)

  # Return content
  return(as.data.frame(content))
}

getProductHistoricRates <- function(product_id, start = NULL, end = NULL,
                                       granularity = NULL, proxy = NULL) {
  # Gets historic rates for a product. Rates are returned in grouped timeslices
  # based on the requested granularity.
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  #   start (numeric):
  #     Requested time period start time in ISO 8601
  #   end (numeric):
  #     Requested time period end time in ISO 8601
  #   granularity (numeric):
  #     Timeslice of data in seconds
  #
  # Returns:
  #   A data frame with columns:
  #     time (POSIXt):
  #       Start time of timeslice
  #     low (numeric):
  #       Lowest price in quote currency units during timeslice 
  #     high (numeric):
  #       Highest price in quote currency units during timeslice
  #     open (numeric):
  #       Timeslice opening price (first trade) in quote currency units
  #     close (numeric):
  #       Timeslice closing price (last trade) in quote currency units
  #     volume (numeric):
  #       Trading volume in base currency units during timeslice 
  
  # Path
  path <- paste("/products/", product_id, "/candles", sep = "")

  # Modify url
  url <- httr::modify_url(url, path = path, query = list("start" = start,
                                                   "end" = end,
                                                   "granularity" = granularity))

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    message <- content$message
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))
  content <- as.data.frame(content[nrow(content):1,])
  rownames(content) <- rev(rownames(content))

  # Change types
  names(content) <- c("time", "low", "high", "open", "close", "volume")
  content$time <- as.POSIXct(content$time, origin = "1970-01-01", tz = "GMT")

  # Return content
  return(as.data.frame(content))
}

getProduct24hStats <- function(product_id, proxy = NULL) {
  # Gets the 24 h stats for the product.
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  #
  # Returns:
  #   A data frame with columns:
  #     open (numeric):
  #       Opening price in quote currency units of last 24 h timeslice
  #     high (numeric):
  #       Highest price in quote currency units during last 24 h timeslice
  #     low (numeric):
  #       Lowest price in quote currency units during last 24 h timeslice
  #     volume (numeric):
  #       Trading volume in base currency units during last 24 h timeslice
  #     last (numeric):
  #       Last trade price in quote currency units
  #     volume_30day (numeric):
  #       Trading volume in base currency units during last 30 day timeslice
  
  # Path
  path <- paste("/products/", product_id, "/stats", sep = "")

  # Modify url
  url <- httr::modify_url(url, path = path)

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    message <- content$message
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))

  # Change non-character types
  content <- lapply(content, as.numeric)

  # Return content
  return(as.data.frame(content))
}

getCurrencies <- function(proxy = NULL) {
  # Gets a list of known currencies.
  #
  # Returns:
  #   A data frame with columns:
  #     id (character):
  #       Currency identifier, e.g. "USD"
  #     name (character):
  #       Currency name, e.g. "United States Dollar"
  #     min_size (numeric):
  #       Currency minimum order/price
  
  # Path
  path <- "/currencies"

  # Modify url
  url <- httr::modify_url(url, path = path)

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    message <- content$message
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))

  # Change non-character types
  content$min_size <- as.numeric(content$min_size)

  # Return content
  return(as.data.frame(content))
}

getTime <- function(proxy = NULL) {
  # Gets the API server time.
  #
  # Returns:
  #   A data frame with columns:
  #     iso:
  #       Time in ISO 8601 format
  #     epoch:
  #       Time in decimal seconds since Unix Epoch
  
  # Path
  path <- "/time"

  # Modify url
  url <- httr::modify_url(url, path = path)

  # GET request
  r <- httr::GET(url, proxy)
  
  # Raise errors
  if (r$status_code != 200) {
    message <- content$message
    content <- jsonlite::fromJSON(httr::content(r, "text"))
    stop(message)
  }

  # Extract content from request
  content <- jsonlite::fromJSON(httr::content(r, "text"))

  # Change non-character types
  content$iso <- strptime(content$iso, "%Y-%m-%dT%H:%M:%OS")

  # Return content
  return(as.data.frame(content))
}

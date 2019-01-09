#### GDAX Public Client API functions ####
# The following packages are required: httr, jsonlite.
# All functions accept an optional proxy argument to use when
# connecting to GDAX (httr::use_proxy() is recommended).

# GDAX API base URL
url <- "https://api.pro.coinbase.com"

#### API Functions ####
getProducts <- function(proxy = NULL) {
  # Gets information about the currency pairs available for trading.
  
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
  content$min_market_funds <- as.numeric(content$min_market_funds)
  content$max_market_funds <- as.numeric(content$max_market_funds)
  content$post_only <- as.logical(content$post_only)
  content$limit_only <- as.logical(content$limit_only)
  content$cancel_only <- as.logical(content$cancel_only)

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
  #     Level of order book detail
  
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
  
  if (content$sequence != 0) {
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
  if (content$trade_id != 0) {
    content$price <- as.numeric(content$price)
    content$size <- as.numeric(content$size)
    content$bid <- as.numeric(content$bid)
    content$ask <- as.numeric(content$ask)
    content$volume <- as.numeric(content$volume)
    content$time <- strptime(content$time, "%Y-%m-%dT%H:%M:%OS")
  }

  # Return content
  return(as.data.frame(content))
}

getProductTrades <- function(product_id, proxy = NULL) {
  # Gets the last 100 trades for a product.
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  
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
  if (length(content) != 0) {
    content <- as.data.frame(content[nrow(content):1,])
    rownames(content) <- rev(rownames(content))
    
    # Change non-character types
    content$time <- strptime(content$time, "%Y-%m-%dT%H:%M:%OS")
    content$price <- as.numeric(content$price)
    content$size <- as.numeric(content$size)
  }

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
  if (length(content) != 0) {
    content <- as.data.frame(content[nrow(content):1,])
    rownames(content) <- rev(rownames(content))
    
    # Change types
    names(content) <- c("time", "low", "high", "open", "close", "volume")
    content$time <- as.POSIXct(content$time, origin = "1970-01-01", tz = "GMT")
    
    # Return content
    return(as.data.frame(content))
  }
}

getProduct24hStats <- function(product_id, proxy = NULL) {
  # Gets the 24 h stats for the product.
  #
  # Arguments:
  #   product_id (character):
  #     Currency pair identifier, e.g. "BTC-USD"
  
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

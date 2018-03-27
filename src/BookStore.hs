module BookStore (

) where

    type BookId = Int
    type MagazineId = Int
    type CustomerId = Int
    type Address = [String]

    data BookInfo = Book 
                        BookId 
                        String 
                        [String] 
                    deriving ( Show )

    data MagazineInfo = Magazine 
                            MagazineId 
                            String 
                            [String] 
                    deriving ( Show)    

    data Customer = Customer {
        customerId :: CustomerId,
        customerName :: String,
        customerAddress :: Address
    } deriving (Show)
                    
    
    bookId :: BookInfo -> BookId
    bookId (Book id _ _) = id 

    title :: BookInfo -> String
    title (Book _ title _) = title

    authors :: BookInfo -> [String]
    authors (Book _ _ authors) = authors

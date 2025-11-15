import Data.Kind (Type)   -- Type - это "вид" (kind) всех обычных типов

-- Creating a distinct type for increased type safety: -- 

newtype Password = Password { unPassword :: String } -- unPassword :: Password -> String ( то есть это функция  по типу геттера , только безопаснее  )
newtype Username = Username { unUsername :: String }

authenticate :: Username -> Password -> Bool
authenticate (Username username) (Password password) = username == "admin" && password == "secret"

--  Wrapping a type to provide specific instances: ( обертывание типа для конкретного экземпляра )

newtype Celsius = Celsius { unCelsius :: Double } deriving (Show, Eq, Ord) -- Предложение deriving позволяет newtypes автоматически выводить экземпляры для распространённых классов типов, таких как Show, Eq и Ord.
 
newtype Fahrenheit = Fahrenheit { unFahrenheit :: Double } deriving (Show, Eq, Ord)

toCelsius :: Fahrenheit -> Celsius
toCelsius (Fahrenheit f) = Celsius ((f - 32) * 5 / 9)

toFahrenheit :: Celsius -> Fahrenheit
toFahrenheit (Celsius c) = Fahrenheit (c * 9 / 5 + 32)


-- Создание «фантомного типа» для гарантий времени компиляции:

data Validated
data Unvalidated

newtype Email (status :: Type) = Email { unEmail :: String }


mkValidatedEmail :: String -> Maybe (Email Validated) -- Smart constructor for creating a validated email
mkValidatedEmail s
  | '@' `elem` s = Just (Email s)
  | otherwise    = Nothing


sendEmail :: Email Validated -> IO () -- A function that only accepts validated emails
sendEmail (Email e) = putStrLn $ "Sending email to: " ++ e

main :: IO ()
main = do
  let validEmailStr = "test@example.com"
      invalidEmailStr = "invalid-email"

  case mkValidatedEmail validEmailStr of
    Just email -> sendEmail email
    Nothing    -> putStrLn "Invalid email format"

  -- sendEmail (Email invalidEmailStr) -- This would be a type error!







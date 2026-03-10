sed -i 's/import Data.Word (Word32)//' tools/ConvertNnue.hs
sed -i 's/import qualified Data.ByteString as BS//' tools/ConvertNnue.hs
sed -i 's/putWord32le (fromIntegral ftIn)/putWord32le (fromIntegral ftIn :: Word32)/' tools/ConvertNnue.hs
sed -i 's/putWord32le (fromIntegral acc)/putWord32le (fromIntegral acc :: Word32)/' tools/ConvertNnue.hs
sed -i 's/putWord32le (fromIntegral hid)/putWord32le (fromIntegral hid :: Word32)/' tools/ConvertNnue.hs

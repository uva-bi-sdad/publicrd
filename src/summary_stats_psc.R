
df <- X2016_data_multiple_usa_spending_transaction_prime_contracts_only_select_fields

df$product_or_service_code <- factor(df$product_or_service_code)

first_letter<-function(x){
return(substring(x,1,1)=='A')
}
x<-first_letter(df$product_or_service_code)
df$is_rd<-x
summary(df$is_rd)
summary(df[df$is_rd,["product_or_service_code"],maxsum=2000)


y<-df %>%
  filter(is_rd)%>%
  select(product_or_service_code,product_or_service_code_description) %>%
  group_by(product_or_service_code)%>%
  count(product_or_service_code_description)

counts<-df%>%
  filter(is_rd)%>%
  count(product_or_service_code)
final<-full_join(counts,y,by="product_or_service_code")

identical(final$n.x,final$n.y)
write.table(final[c("n.x","product_or_service_code",
                    "product_or_service_code_description")],
            "CountsofPSCCodes.csv",
            sep=',')
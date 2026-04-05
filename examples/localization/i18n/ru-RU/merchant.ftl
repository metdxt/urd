### merchant.ftl — русский перевод (ru-RU)
### Сгенерировано из merchant.urd, переведено вручную.
###
### Особенности перевода:
###   • Селектор предметов ($item): health_potion → «зелье здоровья»,
###     mana_potion → «зелье маны»; *[other] — фоллбэк на slug.
###   • Множественное число ($price, $gold): русский язык использует категории
###     one/few/many/other. Для «золотой/золотых» достаточно [one] и *[other].
###   • Падежи: именительный «зелье здоровья», винительный (совпадает для
###     среднего рода), родительный «зелья здоровья» — используются по контексту.

## label: merchant::browse
# @fluent variables: $has_potion (urd: has_potion), $price (urd: price), $gold (urd: gold), $item (urd: item)

# interpolation: $price, $item
merchant-browse-line_1 = Взгляни: { $item ->
        [health_potion] зелье здоровья
        [mana_potion] зелье маны
       *[other] { $item }
    }! Прошу { $price ->
        [one] { $price } золотой
       *[other] { $price } золотых
    } — настоящая находка.

# menu: merchant-browse-menu_1
merchant-browse-menu_1-buy_it_for_price_gold = Купить { $item ->
        [health_potion] зелье здоровья
        [mana_potion] зелье маны
       *[other] { $item }
    } за { $price ->
        [one] { $price } золотой
       *[other] { $price } золотых
    }
merchant-browse-menu_1-try_to_haggle = Поторговаться
merchant-browse-menu_1-never_mind = Не надо

## label: merchant::haggle
# @fluent variables: $has_potion (urd: has_potion), $price (urd: price), $gold (urd: gold)

merchant-haggle-line_1 = Ты умеешь торговаться.
    Двадцать пять золотых — это мой предел. Берёшь или нет?

merchant-haggle-menu_1-line_1 = В другой раз, друг. Счастливого пути.

# menu: merchant-haggle-menu_1
merchant-haggle-menu_1-deal_price_gold = По рукам! { $price ->
        [one] { $price } золотой
       *[other] { $price } золотых
    }
merchant-haggle-menu_1-too_rich_for_my_blood = Слишком дорого

## label: merchant::buy
# @fluent variables: $has_potion (urd: has_potion), $price (urd: price), $gold (urd: gold)

# interpolation: $gold
merchant-buy-if_1-line_1 = Превосходно! Зелье твоё. У тебя теперь { $gold ->
        [one] { $gold } золотой
       *[other] { $gold } золотых
    }.

# interpolation: $gold, $price
merchant-buy-if_1-line_2 = У тебя только { $gold ->
        [one] { $gold } золотой
       *[other] { $gold } золотых
    } — не хватает до { $price ->
        [one] { $price } золотого
       *[other] { $price } золотых
    }. Возвращайся, когда удача улыбнётся.

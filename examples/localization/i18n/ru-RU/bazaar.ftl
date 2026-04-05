### bazaar.ftl — русский перевод (ru-RU)
### Сгенерировано из bazaar.urd, переведено вручную.
###
### Особенности перевода:
###   • Множественное число ($gold): русский язык имеет 4 категории (one/few/many/other).
###     Для «золотой/золотых» достаточно [one] и *[other], так как few/many/other
###     совпадают по форме.
###   • Творительный падеж: «с … золотым / золотыми» — при описании ухода с базара
###     используется склонение прилагательного в творительном падеже.
###   • Селектор предметов ($item): health_potion → «зелье здоровья» (именительный),
###     mana_potion → «зелье маны»; *[other] — фоллбэк на slug.

## label: start
# @fluent variables: $has_potion (urd: has_potion), $price (urd: price), $gold (urd: gold)

bazaar-start-line_1 = Ты входишь на Бродячий Базар.
    Холщовые палатки раскинулись повсюду, освещённые покачивающимися фонарями.

bazaar-start-line_2 = Добро пожаловать! Я Элара, торговка редкими диковинками.
    Не желаешь ли сегодня зелье здоровья?

bazaar-start-menu_1-line_1 = Бродячий Базар следует древним торговым путям.
    Мы появляемся там, где усталые путники нуждаются в нас больше всего.

# menu: bazaar-start-menu_1
bazaar-start-menu_1-browse_the_wares = Посмотреть товары
bazaar-start-menu_1-ask_about_the_bazaar = Спросить о базаре
bazaar-start-menu_1-walk_away = Уйти

## label: farewell
# @fluent variables: $has_potion (urd: has_potion), $price (urd: price), $gold (urd: gold)

# interpolation: $gold
bazaar-farewell-if_1-line_1 = Ты уходишь с зельем в котомке и { $gold ->
        [one] { $gold } золотым
       *[other] { $gold } золотыми
    } в кошельке.

bazaar-farewell-if_1-line_2 = Это зелье спасло немало жизней. Пусть спасёт и твою.

# interpolation: $gold
bazaar-farewell-if_1-line_3 = Ты покидаешь Бродячий Базар с { $gold ->
        [one] { $gold } золотым
       *[other] { $gold } золотыми
    } и неутолённым любопытством.

bazaar-farewell-if_1-line_4 = Возвращайся, когда что-нибудь понадобится. Я буду здесь.

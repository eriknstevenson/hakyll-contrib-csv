# CSV to HTML table generator

## Turn this

```csv
Year,Make,Model,Description,Price
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
```

## Into this

```html
<table width="100%" class="display">
    <thead>
        <tr>
            <td>Year</td>
            <td>Make</td>
            <td>Model</td>
            <td>Description</td>
            <td>Price</td>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>1997</td>
            <td>Ford</td>
            <td>E350</td>
            <td>ac, abs, moon</td>
            <td>3000.00</td>
        </tr>
        <tr>
            <td>1999</td>
            <td>Chevy</td>
            <td>Venture &quot;Extended Edition&quot;</td>
            <td></td>
            <td>4900.00</td>
        </tr>
        <tr>
            <td>1999</td>
            <td>Chevy</td>
            <td>Venture &quot;Extended Edition, Very Large&quot;</td>
            <td></td>
            <td>5000.00</td>
        </tr>
        <tr>
            <td>1996</td>
            <td>Jeep</td>
            <td>Grand Cherokee</td>
            <td>MUST SELL! air, moon roof, loaded</td>
            <td>4799.00</td>
        </tr>
    </tbody>
</table>
```
